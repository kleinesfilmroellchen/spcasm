//! [`AssemblyFile`].

use std::collections::HashMap;
use std::result::Result;
use std::sync::{Arc, Weak};

use miette::SourceSpan;
use parking_lot::RwLock;
#[allow(unused)]
use flexstr::{SharedStr, shared_str, IntoSharedStr, ToSharedStr};

use super::instruction::{Instruction, MemoryAddress, Opcode};
use super::reference::{
	Label, MacroParameters, MacroParent, Reference, ReferenceResolvable, RelativeReferenceDirection,
};
use super::{AddressingMode, AssemblyTimeValue, Environment, ProgramElement};
use crate::assembler::resolve_file;
use crate::directive::DirectiveValue;
use crate::error::AssemblyError;
use crate::parser::{lex, Token};
use crate::{AssemblyCode, Change, Directive, Segments};

/// The AST and associated information for one file.
#[derive(Debug)]
#[allow(clippy::module_name_repetitions)]
pub struct AssemblyFile {
	/// Parsed contents.
	pub content:     Vec<ProgramElement>,
	/// Underlying source code and file name.
	pub source_code: Arc<AssemblyCode>,
	/// The environment that this file is parsed in.
	pub parent:      Weak<RwLock<Environment>>,
	/// The tokens that were parsed for this file.
	pub tokens:      Vec<Token>,
}

impl AssemblyFile {
	/// Returns the token at the given offset, if any.
	#[must_use]
	pub fn token_at(&self, offset: usize) -> Option<Token> {
		self.tokens.iter().find_map(|token| {
			let span = token.source_span();
			if Self::source_span_contains(span, offset) {
				Some(token.clone())
			} else {
				None
			}
		})
	}

	fn source_span_contains(this: SourceSpan, offset: usize) -> bool {
		this.offset() <= offset && this.offset() + this.len() > offset
	}

	/// Returns the reference defined at the specified offset, if there is one.
	#[must_use]
	pub fn reference_at(&self, offset: usize) -> Option<Reference> {
		Self::reference_at_offset_in(self.parent.upgrade()?.try_read()?.globals.values(), offset)
	}

	/// Returns the reference defined at the specified offset, searching in the given iterator.
	#[must_use]
	fn reference_at_offset_in<'a>(
		references: impl Iterator<Item = &'a Arc<RwLock<Label>>>,
		offset: usize,
	) -> Option<Reference> {
		for reference in references {
			if let Some(reading_reference) = reference.try_read() {
				if reading_reference
					.definition_span
					.is_some_and(|definition| Self::source_span_contains(definition, offset))
					|| reading_reference.usage_spans.iter().any(|usage| Self::source_span_contains(*usage, offset))
				{
					return Some(Reference::Label(reference.clone()));
				}
				if let Some(child_reference) = Self::reference_at_offset_in(reading_reference.children.values(), offset)
				{
					return Some(child_reference);
				}
			}
		}
		None
	}

	/// Returns the definitions of the given identifier string, if it is defined as a symbol anywhere.
	/// Note that things like local labels and relative references can be defined in multiple places.
	#[must_use]
	pub fn get_definition_spans_of(&self, identifier: &str) -> Vec<SourceSpan> {
		self.content
			.iter()
			.filter_map(|element| match element {
				ProgramElement::Label(label) if label.name() == identifier => Some(label.source_span()),
				ProgramElement::Directive(Directive {
					value: DirectiveValue::AssignReference { reference, .. },
					span,
					..
				}) if reference.name() == identifier => Some(*span),
				ProgramElement::Directive(Directive {
					value: DirectiveValue::UserDefinedMacro { name, .. },
					span,
					..
				}) if name == identifier => Some(*span),
				_ => None,
			})
			.collect()
	}

	/// Fills in the global label references for all local labels. Existing ones are overwritten, so the labels are
	/// always consistent.
	///
	/// # Errors
	/// If a local label precedes any global labels.
	/// # Panics
	/// All panics are programming errors.
	#[allow(clippy::too_many_lines)]
	pub fn fill_in_reference_links(&mut self) -> Result<(), Box<AssemblyError>> {
		let mut current_label: Option<Arc<RwLock<Label>>> = None;
		// While we do the local label merging, also perform the backward relative resolution.
		// The forward relative labels are resolved in a reverse pass afterwards.
		let mut current_backward_relative_label_map = HashMap::new();

		for element in &mut self.content {
			// First match for reference resolution in instruction position
			match element {
				ProgramElement::Label(ref mal @ Reference::MacroArgument { ref span, .. }) =>
					return Err(AssemblyError::UsingMacroArgumentOutsideMacro {
						name:     mal.to_string().into(),
						src:      self.source_code.clone(),
						location: *span,
					}
					.into()),
				ProgramElement::Label(Reference::Relative {
					direction: RelativeReferenceDirection::Backward,
					id,
					span,
					..
				}) => {
					// "Drop" the mutable borrow from id before *element is used to not confuse the borrow checker.
					let id = *id;
					// To reference the relative label until codegen, create a new local label for it.
					// This name is likely, but not guaranteed, to be unique! That's why we directly insert into
					// the globals list.
					let label_name:SharedStr =
						format!("{}_{}", "-".repeat(usize::try_from(u64::from(id)).unwrap()), span.offset()).into();
					let global_for_relative = Label::new_synthetic(label_name.clone(), *span);
					self.parent
						.upgrade()
						.expect("parent disappeared")
						.write()
						.globals
						.insert(label_name, global_for_relative.clone());
					*element = ProgramElement::Label(Reference::Label(global_for_relative.clone()));
					current_backward_relative_label_map.insert(id, global_for_relative);
				},

				ProgramElement::UserDefinedMacroCall { .. }
				| ProgramElement::IncludeSource { .. }
				| ProgramElement::Label(
					Reference::Label(_)
					| Reference::MacroGlobal { .. }
					| Reference::Relative { .. }
					| Reference::UnresolvedLocalLabel { .. },
				)
				| ProgramElement::Directive(_)
				| ProgramElement::Instruction(_) => (),
			}
			element.set_current_label(&current_label, &self.source_code)?;
			// Set any non-synthetic label as the current label.
			if let ProgramElement::Label(Reference::Label(ref label)) = element && !label.read().synthetic {
				current_label = Some(label.clone());
			}
			element.resolve_relative_labels(RelativeReferenceDirection::Backward, &current_backward_relative_label_map);
		}

		let mut current_forward_relative_label_map = HashMap::new();
		// Reverse iteration to resolve forward references.
		// Also use this second pass to resolve hierarchical local label shorthands (`global_local` for referencing
		// `global: .local: ...`)
		for element in self.content.iter_mut().rev() {
			if let ProgramElement::Label(Reference::Relative {
				direction: RelativeReferenceDirection::Forward,
				id,
				span,
				..
			}) = element
			{
				let id = *id;
				// To reference the relative label until codegen, create a new local label for it.
				let label_name:SharedStr =
					format!("{}_{}", "+".repeat(usize::try_from(u64::from(id)).unwrap()), span.offset()).into();
				let global_for_relative = Label::new_synthetic(label_name.clone(), *span);
				self.parent
					.upgrade()
					.expect("parent disappeared")
					.write()
					.globals
					.insert(label_name, global_for_relative.clone());
				*element = ProgramElement::Label(Reference::Label(global_for_relative.clone()));
				current_forward_relative_label_map.insert(id, global_for_relative);
			}
			element.resolve_relative_labels(RelativeReferenceDirection::Forward, &current_forward_relative_label_map);
		}

		// Final forward iteration to resolve sub-label reference syntax, e.g. `global_local` for the label hierarchy
		// `global: .local: ...`
		let strong_parent = self.parent.upgrade().expect("parent disappeared");
		let global_labels: Vec<_> = strong_parent.read().globals.values().cloned().collect();
		for element in &mut self.content {
			element.resolve_pseudo_labels(&global_labels);
		}

		Ok(())
	}

	/// Fills in references to macro argument lists for macro argument occurrences.
	///
	/// # Errors
	/// If a macro argument with a wrong name was encountered.
	pub fn resolve_user_macro_arguments(&mut self) -> Result<(), Box<AssemblyError>> {
		for element in &mut self.content {
			if let ProgramElement::Directive(Directive {
				value: DirectiveValue::UserDefinedMacro { ref arguments, body, .. },
				..
			}) = element
			{
				for child_element in body {
					child_element.replace_macro_parent(arguments.clone(), &self.source_code)?;
				}
			}
		}
		Ok(())
	}

	/// Tries to coerce addressing modes to direct page addressing wherever possible. This needs to be done again as the
	/// unresolved local labels did not provide memory locations before merging.
	pub fn coerce_to_direct_page_addressing(&mut self) {
		for element in &mut self.content {
			if let ProgramElement::Instruction(Instruction {
				opcode: Opcode { first_operand, second_operand, force_direct_page, .. },
				..
			}) = element
			{
				let coercion_function = if *force_direct_page {
					AddressingMode::force_to_direct_page_addressing
				} else {
					AddressingMode::coerce_to_direct_page_addressing
				};
				*first_operand = first_operand.clone().map(AddressingMode::optimize_numbers).map(coercion_function);
				*second_operand = second_operand.clone().map(AddressingMode::optimize_numbers).map(coercion_function);
			}
		}
	}

	fn to_asm_error<'a>(
		span: &'a SourceSpan,
		source_code: &'a Arc<AssemblyCode>,
	) -> impl Fn(()) -> Box<AssemblyError> + 'a {
		|_| AssemblyError::MissingSegment { location: *span, src: source_code.clone() }.into()
	}

	/// Splits the AST into segments which (still) contain [`ProgramElement`]s. This is an initial step in laying out
	/// segments and their contents, and also helps to catch user segmenting errors early.
	///
	/// # Errors
	/// Any error that occurs during segment splitting.
	#[allow(clippy::missing_panics_doc)]
	pub fn split_into_segments(&self) -> Result<Segments<ProgramElement>, Box<AssemblyError>> {
		let mut segments = Segments::default();
		let mut brr_label_number = 0;
		let mut current_labels = Vec::default();
		for mut element in self.content.iter().cloned() {
			match element {
				ProgramElement::Label(reference) => {
					current_labels.push(reference.clone());
					if segments.add_element(ProgramElement::Label(reference.clone())).is_err() {
						// Missing segment at label insertion point; the label will be undefined but that doesn't
						// matter.
						self.parent.upgrade().unwrap().read().options.report_diagnostic(
							AssemblyError::ReferenceOutsideSegment {
								reference: reference.name(),
								location:  reference.source_span(),
								src:       self.source_code.clone(),
							},
						);
					}
					// Prevent the clearing of current labels.
					continue;
				},
				ProgramElement::Instruction(instruction) => segments
					.add_element(ProgramElement::Instruction(instruction.clone()))
					.map_err(Self::to_asm_error(&instruction.span, &self.source_code))?,
				ProgramElement::Directive(Directive { value: DirectiveValue::End, .. }) => break,
				ProgramElement::Directive(ref mut directive @ Directive { .. }) => {
					// All BRR directives that go into the BRR sample directory need a label, so we add a unique label
					// while we're here.
					if matches!(&directive.value, DirectiveValue::Brr { directory: true, .. })
						&& current_labels.is_empty()
					{
						let label_name:SharedStr = format!("brr_sample_{}", brr_label_number).into();
						let new_brr_label = Label::new_synthetic(label_name.clone(), directive.span);
						brr_label_number += 1;

						self.parent
							.upgrade()
							.ok_or_else(|| panic!("parent disappeared"))
							.unwrap()
							.write()
							.globals
							.insert(label_name, new_brr_label.clone());
						segments
							.add_element(ProgramElement::Label(Reference::Label(new_brr_label.clone())))
							.map_err(Self::to_asm_error(&new_brr_label.read().source_span(), &self.source_code))?;
						current_labels.push(Reference::Label(new_brr_label));
					}

					directive.perform_segment_operations_if_necessary(
						&mut segments,
						self.source_code.clone(),
						current_labels.first(),
					)?;
					if !directive.value.is_symbolic() {
						segments
							.add_element(ProgramElement::Directive(directive.clone()))
							.map_err(Self::to_asm_error(&directive.span, &self.source_code))?;
					}
				},
				ProgramElement::IncludeSource { .. } | ProgramElement::UserDefinedMacroCall { .. } => {},
			}
			current_labels.clear();
		}

		self.optimize_direct_page_labels(&mut segments);

		Ok(segments)
	}

	/// Optimizes long addressing instructions to use direct page addressing if the reference is in the direct page.
	/// This involves non-trivial semantic analysis:
	///
	/// 1. Collect all references, and all non-direct-page instructions referring to them; and their current
	/// locations.
	/// General hint: Calculate new reference positions by subtracting from every reference's position the number of
	/// dp-coerced instructions.
	/// 2. Assume all references lie in the direct page; i.e. the instructions can shrink by 1 byte.
	/// 3. Repeatedly find references (or reference-based calculations) that do not lie in the direct page anymore, and
	/// update instructions and positions. Repeat until no changes happen or the reference resolution limit is reached.
	/// 4. Modify instructions accordingly
	///
	/// TODO: This function is very hot; >20% of runtime. Optimize the optimizer :^)
	#[allow(clippy::too_many_lines)]
	fn optimize_direct_page_labels(&self, segments: &mut Segments<ProgramElement>) {
		/// Do the more complex direct page coercing with references. We only consider references in the direct page if
		/// they are in the *zero page*. If that is a problem, forcing to direct page addressing is always possible.
		#[derive(Debug, Clone)]
		enum InstructionOrReference {
			Instruction {
				/// The index is required to recover the actual instruction and modify it later on. It is purely
				/// symbolic and has nothing to do with the instruction's address.
				index_in_segment: usize,
				// Each reference also stores the exact calculation that it is used in. Even if the reference itself is
				// in the direct page, the calculated location may not be!
				references:       Vec<(Reference, AssemblyTimeValue)>,
				// Whether this instruction is (now) short, i.e. in direct page mode.
				is_short:         bool,
			},
			Reference(Reference),
		}

		#[derive(Debug, Clone)]
		struct ReferencedObject {
			// Currently expected address of this referenced object.
			address:       MemoryAddress,
			/// Start address of the segment this referenced object is within.
			segment_start: MemoryAddress,
			object:        InstructionOrReference,
		}

		let max_coercion_passes =
			self.parent.upgrade().expect("parent disappeared").read().options.maximum_reference_resolution_passes();

		// 1. (collect relevant objects)
		let mut referenced_objects = Vec::<ReferencedObject>::new();

		for (segment_start, segment_contents) in &segments.segments {
			for (index_in_segment, (element, offset)) in segment_contents
				.iter()
				.scan(0.into(), |offset: &mut MemoryAddress, element| {
					let return_value = Some((element, *offset));
					*offset += element.assembled_size() as MemoryAddress;
					return_value
				})
				.enumerate()
			{
				match element {
					ProgramElement::Label(label) => referenced_objects.push(ReferencedObject {
						address:       offset + *segment_start,
						segment_start: *segment_start,
						object:        InstructionOrReference::Reference(label.clone()),
					}),
					ProgramElement::Instruction(Instruction { opcode, .. }) => {
						let references = opcode.references_and_calculations();
						if opcode.has_long_address()
							&& !references.iter().all(|(_, value)| value.is_resolved())
							&& opcode.can_use_direct_page_addressing()
						{
							referenced_objects.push(ReferencedObject {
								address:       offset + *segment_start,
								segment_start: *segment_start,
								object:        InstructionOrReference::Instruction {
									index_in_segment,
									references: references
										.into_iter()
										.map(|(reference, value)| (reference.clone(), value.clone()))
										.collect(),
									is_short: false,
								},
							});
						}
					},
					ProgramElement::Directive(Directive { .. }) => (),
					ProgramElement::IncludeSource { .. } | ProgramElement::UserDefinedMacroCall { .. } => panic!(
						"source includes and unresolved macro calls at reference optimization time, this is a bug"
					),
				}
			}
		}

		// 2. (assume direct page references everywhere)
		// Store by how much later objects need to be offset forwards.
		let mut address_offset = 0;
		let mut last_segment = None;
		for ReferencedObject { address, segment_start, object } in &mut referenced_objects {
			// New segment started, let's reset the address offset since segments don't influence each other.
			if let Some(last_segment) = last_segment && *segment_start != last_segment {
				address_offset = 0;
			}
			*address += address_offset;
			if let InstructionOrReference::Instruction { is_short, .. } = object {
				address_offset -= 1;
				*is_short = true;
			}
			last_segment = Some(*segment_start);
		}

		let mut iteration = 1;
		let mut change = true;
		// 3.
		while change && iteration <= max_coercion_passes {
			change = false;

			let all_references = referenced_objects
				.iter()
				.filter_map(|ReferencedObject { object: candidate, address, .. }| match candidate {
					InstructionOrReference::Reference(reference) => Some((*address, reference.clone())),
					InstructionOrReference::Instruction { .. } => None,
				})
				.collect::<Vec<_>>();
			let find_value_for_reference = |queried_reference| {
				for (address, candidate) in &all_references {
					if candidate == &queried_reference {
						return Some(*address);
					}
				}
				None
			};

			let mut address_offset = 0;
			let mut last_segment = None;
			let mut index: isize = 0;
			#[allow(clippy::cast_sign_loss)]
			while index < referenced_objects.len().try_into().unwrap() {
				let ReferencedObject { address, object, segment_start } = &mut referenced_objects[index as usize];
				let segment_start = *segment_start;
				if let Some(last_segment) = last_segment && segment_start != last_segment {
					address_offset = 0;
				}
				*address += address_offset;
				match object {
					InstructionOrReference::Instruction { references, is_short, .. }
						if references.iter().any(|(_, value)| {
							// Usefully, if the resolver can't help with some of the references, we just fall back to
							// long addressing mode automatically.
							matches!(value.value_using_resolver(&find_value_for_reference), Some(0x100 ..) | None)
						}) =>
					{
						change = true;
						*is_short = false;
						address_offset += 1;
						// This object was just unoptimized, no need to consider it in future runs.
						referenced_objects.remove(index as usize);
						index -= 1;
					},
					_ => {},
				}
				last_segment = Some(segment_start);
				index += 1;
			}

			iteration += 1;
		}

		// 4.
		for (segment_start, index_in_segment) in
			referenced_objects.iter().filter_map(|ReferencedObject { segment_start, object, .. }| match object {
				InstructionOrReference::Instruction { index_in_segment, is_short: true, .. } =>
					Some((*segment_start, *index_in_segment)),
				_ => None,
			}) {
			let instruction_segment = segments.segments.get_mut(&segment_start).unwrap();
			if let ProgramElement::Instruction(instruction) = &mut instruction_segment[index_in_segment] {
				if let Some(op) = instruction.opcode.first_operand.as_mut() {
					*op = op.clone().force_to_direct_page_addressing();
				}
				if let Some(op) = instruction.opcode.second_operand.as_mut() {
					*op = op.clone().force_to_direct_page_addressing();
				}
			}
		}
	}

	/// Returns whether this file's parsed content contains any unresolved include directives.
	pub(crate) fn has_unresolved_source_includes(&self) -> bool {
		self.content.iter().any(|element| matches!(element, ProgramElement::IncludeSource { .. }))
	}

	/// Resolves all source include directives by recursively calling into lexer and parser.
	///
	/// # Errors
	/// All errors from other files are propagated, as well as include cycles.
	pub fn resolve_source_includes(&mut self) -> Result<(), Box<AssemblyError>> {
		let mut index = 0;
		while index < self.content.len() {
			let element = self.content[index].clone();
			if let ProgramElement::IncludeSource { ref file, span } = element {
				let environment = self.parent.upgrade().expect("parent deleted while we're still parsing");
				let file:SharedStr = resolve_file(&self.source_code, file).to_string_lossy().as_ref().into();
				let mut included_code =
					AssemblyCode::from_file(&file).map_err(|os_error| AssemblyError::FileNotFound {
						os_error:  Arc::new(os_error),
						file_name: file,
						src:       self.source_code.clone(),
						location:  span,
					})?;
				let child_include_path = &mut unsafe { Arc::get_mut_unchecked(&mut included_code) }.include_path;
				child_include_path.push(self.source_code.name.clone());
				child_include_path.append(&mut self.source_code.include_path.clone());

				let tokens = lex(included_code.clone(), &*environment.read().options)?;
				let included_file = Environment::parse(&environment, tokens, &included_code)?;

				self.content.splice(index ..= index, included_file.read().content.clone());
				continue;
			}
			index += 1;
		}
		Ok(())
	}

	/// Expands calls to user-defined macros.
	///
	/// # Errors
	/// Any errors relating to macro calls and macro definitions.
	pub(super) fn expand_user_macros(&mut self) -> Result<Change, Box<AssemblyError>> {
		let maximum_macro_expansion_depth = self
			.parent
			.upgrade()
			.expect("environment destroyed before assembly file")
			.read()
			.options
			.maximum_macro_expansion_depth();

		let user_macros = self
			.content
			.iter()
			.filter_map(|el| match el {
				ProgramElement::Directive(Directive {
					span,
					value: value @ DirectiveValue::UserDefinedMacro { name, .. },
					..
				}) => Some((name.clone(), (*span, value.clone()))),
				_ => None,
			})
			.collect::<HashMap<_, _>>();

		let mut index = 0;
		// A stack of end indices where code inserted by macros ends. Specifically, the indices point at the first
		// program element after the macro. This is used to keep track of recursion depth.
		let mut macro_end_stack = Vec::new();

		while index < self.content.len() {
			let element = &mut self.content[index];

			if let ProgramElement::UserDefinedMacroCall { macro_name, arguments: actual_arguments, span, .. } = element
			{
				if macro_end_stack.len() > maximum_macro_expansion_depth {
					return Err(AssemblyError::RecursiveMacroUse {
						depth:    maximum_macro_expansion_depth,
						name:     macro_name.clone(),
						location: *span,
						src:      self.source_code.clone(),
					}
					.into());
				}

				let called_macro = user_macros.get(macro_name);
				if let Some((definition_span, DirectiveValue::UserDefinedMacro { arguments, body, .. })) = called_macro
				{
					let arguments = arguments.read();
					let formal_arguments = match &(arguments).parameters {
						MacroParameters::Formal(formal_arguments) => formal_arguments,
						MacroParameters::Actual(_) => unreachable!(),
					};
					if formal_arguments.len() != actual_arguments.len() {
						return Err(AssemblyError::IncorrectNumberOfMacroArguments {
							name:            macro_name.clone(),
							expected_number: formal_arguments.len(),
							actual_number:   actual_arguments.len(),
							location:        *span,
							definition:      *definition_span,
							src:             self.source_code.clone(),
						}
						.into());
					}
					let actual_argument_parent = MacroParent::new_actual(
						formal_arguments
							.iter()
							.zip(actual_arguments.iter())
							.map(|((formal_argument, _), actual_argument)| {
								(formal_argument.clone(), actual_argument.clone())
							})
							.collect(),
						// We use a unique reference name just to make sure that we don't combine different
						// references accidentally. This is not a synthetic label!
						Label::new_with_definition(
							format!("{}_global_label_{}", macro_name, index).into(),
							*definition_span,
						),
					);
					// FIXME: Doesn't handle macro-internal references correctly; also no support for the \@ special
					// label.
					let mut inserted_body = body.clone();
					for macro_element in &mut inserted_body {
						macro_element.replace_macro_parent(actual_argument_parent.clone(), &self.source_code)?;
					}

					let body_length = inserted_body.len();
					self.content.splice(index ..= index, inserted_body);

					// Shift all later end indices backwards to account for the inserted instructions.
					macro_end_stack = macro_end_stack
						.into_iter()
						.map(|end_index| if end_index >= index { end_index + body_length } else { end_index })
						.collect();
					macro_end_stack.push(index + body_length);
					continue;
				}
				return Err(AssemblyError::UndefinedUserMacro {
					name:             macro_name.clone(),
					available_macros: user_macros.keys().map(SharedStr::clone).collect(),
					location:         *span,
					src:              self.source_code.clone(),
				}
				.into());
			}
			index += 1;
			// Using drain_filter is the easiest way of filtering elements from a vector. We need to consume the
			// returned iterator fully or else not all filtering will happen.
			let _: usize = macro_end_stack.extract_if(|end_index| *end_index < index).count();
		}

		Ok(Change::Unmodified)
	}
}
