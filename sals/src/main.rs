#![doc = include_str!("../README.md")]
#![feature(try_blocks)]
#![allow(
	clippy::wildcard_imports,
	clippy::ignored_unit_patterns,
	clippy::needless_raw_string_hashes,
	clippy::doc_markdown
)]

use std::path::Path;
use std::sync::Arc;

use flexstr::SharedStr;
use interface::*;
use miette::SourceSpan;
use parking_lot::{Mutex, RwLock};
use semantic_token::*;
use serde_json::Value;
use spcasm::cli::Frontend;
use spcasm::parser::Token;
use spcasm::sema::AssemblyFile;
use spcasm::{AssemblyCode, AssemblyError, Environment};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

mod interface;
mod semantic_token;

shadow_rs::shadow!(buildinfo);

#[derive(Debug)]
struct Backend {
	client:      Client,
	frontend:    Arc<ServerFrontend>,
	environment: Arc<RwLock<Environment>>,
}

#[derive(Debug)]
struct ServerFrontend {
	collected_diagnostics: Mutex<Vec<AssemblyError>>,
}

impl ServerFrontend {
	pub fn clear_diagnostics(&self) {
		self.collected_diagnostics.lock().clear();
	}
}

impl Default for ServerFrontend {
	fn default() -> Self {
		Self { collected_diagnostics: Mutex::new(Vec::new()) }
	}
}

impl Frontend for ServerFrontend {
	fn is_error(&self, _warning: &spcasm::AssemblyError) -> bool {
		false
	}

	fn is_ignored(&self, _warning: &spcasm::AssemblyError) -> bool {
		false
	}

	fn maximum_macro_expansion_depth(&self) -> usize {
		10000
	}

	fn maximum_reference_resolution_passes(&self) -> usize {
		100
	}

	fn report_diagnostic_impl(&self, diagnostic: AssemblyError) {
		self.collected_diagnostics.lock().push(diagnostic);
	}
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
	async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
		self.client
			.log_message(
				MessageType::INFO,
				format!(
					"sals server {}, built {}, {}",
					buildinfo::PKG_VERSION,
					buildinfo::BUILD_TIME,
					buildinfo::RUST_VERSION
				),
			)
			.await;
		Ok(InitializeResult {
			server_info:     Some(ServerInfo {
				name:    "sals".to_string(),
				version: Some(buildinfo::PKG_VERSION.to_string()),
			}),
			offset_encoding: None,
			capabilities:    ServerCapabilities {
				// FIXME: Use UTF-8 once the client shim supports it: https://github.com/microsoft/vscode-languageserver-node/issues/1224
				// For an explanation of why this is important, see https://fasterthanli.me/articles/the-bottom-emoji-breaks-rust-analyzer#the-way-forward
				// Please do not file a bug if this causes a UTF-8-related panic; sals and spcasm will never ever handle
				// UTF-16 ever.
				position_encoding: Some(PositionEncodingKind::UTF16),
				inlay_hint_provider: None,
				text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
				// completion_provider: Some(CompletionOptions {
				// 	resolve_provider:           Some(false),
				// 	trigger_characters:         Some(vec![",".to_string(), ".".to_string()]),
				// 	work_done_progress_options: Default::default(),
				// 	all_commit_characters:      Some(vec!["\n".to_string(), " ".to_string()]),
				// 	completion_item:            None,
				// }),
				completion_provider: None,
				execute_command_provider: None,

				workspace: Some(WorkspaceServerCapabilities {
					workspace_folders: Some(WorkspaceFoldersServerCapabilities {
						supported:            Some(false),
						change_notifications: Some(OneOf::Left(true)),
					}),
					file_operations:   None,
				}),
				semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
					SemanticTokensRegistrationOptions {
						text_document_registration_options: {
							TextDocumentRegistrationOptions {
								document_selector: Some(vec![DocumentFilter {
									language: Some("spc700".to_string()),
									scheme:   Some("file".to_string()),
									pattern:  None,
								}]),
							}
						},
						semantic_tokens_options:            SemanticTokensOptions {
							work_done_progress_options: WorkDoneProgressOptions { work_done_progress: Some(false) },
							legend:                     SemanticTokensLegend {
								token_types:     SEMANTIC_TOKEN_TYPES.map(std::convert::Into::into).into(),
								token_modifiers: Vec::new(),
							},
							range:                      Some(false),
							full:                       Some(SemanticTokensFullOptions::Bool(true)),
						},
						static_registration_options:        StaticRegistrationOptions::default(),
					},
				)),
				definition_provider: Some(OneOf::Left(true)),
				references_provider: Some(OneOf::Left(true)),

				..ServerCapabilities::default()
			},
		})
	}

	async fn initialized(&self, _: InitializedParams) {
		self.client.log_message(MessageType::INFO, "initialized!").await;
	}

	async fn shutdown(&self) -> Result<()> {
		self.client
			.log_message(MessageType::INFO, format!("sals server {} shutting down.", buildinfo::PKG_VERSION))
			.await;
		Ok(())
	}

	async fn did_open(&self, params: DidOpenTextDocumentParams) {
		self.on_change(TextDocumentItem {
			uri:     params.text_document.uri,
			text:    params.text_document.text.into(),
			version: params.text_document.version,
		})
		.await;
	}

	async fn did_change(&self, params: DidChangeTextDocumentParams) {
		if params.content_changes.len() == 1 {
			self.on_change(TextDocumentItem {
				uri:     params.text_document.uri,
				text:    (&params.content_changes[0].text).into(),
				version: params.text_document.version,
			})
			.await;
		} else {
			self.client
				.log_message(
					MessageType::ERROR,
					format!(
						"More than one change ({}) received for document {}",
						params.text_document.uri,
						params.content_changes.len(),
					),
				)
				.await;
		}
	}

	async fn did_save(&self, _: DidSaveTextDocumentParams) {}

	async fn did_close(&self, _: DidCloseTextDocumentParams) {
		self.client.log_message(MessageType::INFO, "file closed!").await;
	}

	async fn goto_definition(&self, params: GotoDefinitionParams) -> Result<Option<GotoDefinitionResponse>> {
		let file = self.file_for_uri(&params.text_document_position_params.text_document.uri)?;
		let definition = async {
			let source_code = file.read().source_code.text.clone();

			let offset = lsp_position_to_source_offset(params.text_document_position_params.position, &source_code);

			// Use reference itself if it can be found.
			#[allow(clippy::significant_drop_in_scrutinee)]
			let spans = if let Some(reference) = file.read().reference_at(offset) {
				vec![reference.source_span()]
			} else {
				// Fall back to token-based search otherwise.

				let text = file.read().token_at(offset).and_then(|token| match token {
					Token::Identifier(text, _) => Some(text),
					_ => None,
				})?;

				file.read().get_definition_spans_of(&text)
			}
			.into_iter()
			.filter_map(|span| {
				Some(Location::new(
					params.text_document_position_params.text_document.uri.clone(),
					Range::new(
						source_offset_to_lsp_position(span.offset(), &source_code)?,
						source_offset_to_lsp_position(span.offset() + span.len(), &source_code)?,
					),
				))
			})
			.collect::<Vec<_>>();

			self.client.log_message(MessageType::INFO, &format!("{spans:?}")).await;
			match spans.len() {
				0 => None,
				1 => Some(GotoDefinitionResponse::Scalar(spans[0].clone())),
				_ => Some(GotoDefinitionResponse::Array(spans)),
			}
		}
		.await;
		Ok(definition)
	}

	async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
		use spcasm::sema::Reference;

		let file = self.file_for_uri(&params.text_document_position.text_document.uri)?;
		let offset =
			lsp_position_to_source_offset(params.text_document_position.position, &file.read().source_code.text);
		let readable_file = file.read();

		readable_file.reference_at(offset).map_or(Ok(None), |reference| {
			let spans: Box<dyn Iterator<Item = SourceSpan>> = match reference {
				Reference::Label(label) =>
					Box::new(label.read().usage_spans.clone().into_iter().chain(label.read_recursive().definition_span)),
				Reference::MacroArgument { span, .. }
				| Reference::MacroGlobal { span, .. }
				| Reference::Relative { span, .. }
				| Reference::UnresolvedLabel { span, .. } => Box::new(std::iter::once(span)),
			};

			// TODO: Cross-file locations aren't handled properly.
			Ok(Some(
				spans
					.filter_map(|span| {
						Some(Location::new(
							params.text_document_position.text_document.uri.clone(),
							Range::new(
								source_offset_to_lsp_position(span.offset(), &readable_file.source_code.text)?,
								source_offset_to_lsp_position(
									span.offset() + span.len(),
									&readable_file.source_code.text,
								)?,
							),
						))
					})
					.collect(),
			))
		})
	}

	async fn semantic_tokens_full(&self, params: SemanticTokensParams) -> Result<Option<SemanticTokensResult>> {
		let file = self.file_for_uri(&params.text_document.uri)?;
		let semantic_tokens = {
			let text = file.read().source_code.text.clone();
			// Necessary for semantic token location delta computation.
			let mut last_token_line = 0;
			let mut last_token_column = 0;
			let tokens = file
				.read()
				.tokens
				.iter()
				.filter_map(|token| {
					if matches!(token, Token::Newline(_)) {
						return None;
					}
					let start_location = source_offset_to_lsp_position(token.source_span().offset(), &text)?;
					// https://github.com/microsoft/vscode-extension-samples/blob/5ae1f7787122812dcc84e37427ca90af5ee09f14/semantic-tokens-sample/vscode.proposed.d.ts#L71
					let delta_start = if start_location.line == last_token_line {
						start_location.character - last_token_column
					} else {
						start_location.character
					};
					let semantic_token = Some(SemanticToken {
						delta_line: start_location.line - last_token_line,
						delta_start,
						length: u32::try_from(token.source_span().len()).ok()?,
						token_type: SpcasmTokenType::from(token).try_into().ok()?,
						token_modifiers_bitset: 0,
					});
					last_token_line = start_location.line;
					last_token_column = start_location.character;
					semantic_token
				})
				.collect::<Vec<_>>();
			Some(tokens)
		};
		if let Some(semantic_token) = semantic_tokens {
			return Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
				result_id: None,
				data:      semantic_token,
			})));
		}
		Ok(None)
	}

	async fn semantic_tokens_range(
		&self,
		params: SemanticTokensRangeParams,
	) -> Result<Option<SemanticTokensRangeResult>> {
		self.client
			.log_message(MessageType::WARNING, format!("semantic tokens range {params:?} recieved but not supported."))
			.await;
		Ok(None)
	}

	async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
		log::info!("completion request {:?}!", params);
		Ok(None)
	}

	async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
		log::info!("rename request {:?}!", params);
		Ok(None)
	}

	async fn did_change_configuration(&self, _: DidChangeConfigurationParams) {
		self.client.log_message(MessageType::INFO, "configuration changed!").await;
	}

	async fn did_change_workspace_folders(&self, _: DidChangeWorkspaceFoldersParams) {
		self.client.log_message(MessageType::INFO, "workspace folders changed!").await;
	}

	async fn did_change_watched_files(&self, _: DidChangeWatchedFilesParams) {
		self.client.log_message(MessageType::INFO, "watched files have changed!").await;
	}

	async fn execute_command(&self, _: ExecuteCommandParams) -> Result<Option<Value>> {
		self.client.log_message(MessageType::INFO, "command executed!").await;

		match self.client.apply_edit(WorkspaceEdit::default()).await {
			Ok(res) if res.applied => self.client.log_message(MessageType::INFO, "applied").await,
			Ok(_) => self.client.log_message(MessageType::INFO, "rejected").await,
			Err(err) => self.client.log_message(MessageType::ERROR, err).await,
		}

		Ok(None)
	}
}

struct TextDocumentItem {
	uri:     Url,
	text:    SharedStr,
	version: i32,
}

impl Backend {
	async fn on_change(&self, TextDocumentItem { uri, text, version }: TextDocumentItem) {
		if let Ok(path) = &uri.to_file_path() {
			// Clear frontend diagnostics.
			self.frontend.clear_diagnostics();
			let source_code = Arc::new(AssemblyCode::new_from_path(&text, path));

			// Reset relevant parts of the environment: This file as well as the global list (to avoid redefinition
			// errors). Do this in a separate block to ensure the borrow checker that environment does not leak into the
			// returned `Future`.
			{
				let mut environment = self.environment.write();
				environment.files.remove(&source_code.name);
				environment.globals.clear();
			}

			let result = try {
				let tokens = spcasm::parser::lex(source_code.clone(), &*self.frontend).map_err(AssemblyError::from)?;
				let program =
					Environment::parse(&self.environment, tokens, &source_code).map_err(AssemblyError::from)?;
				let mut segmented_program = program.write().split_into_segments().map_err(AssemblyError::from)?;
				let _assembled = spcasm::assembler::assemble_inside_segments(
					&mut segmented_program,
					&source_code,
					self.frontend.clone(),
				)
				.map_err(AssemblyError::from)?;
			};
			if let Err(error) = result {
				let diagnostics = assembly_error_to_lsp_diagnostics(&error, &text);
				self.client.publish_diagnostics(uri.clone(), diagnostics, Some(version)).await;
			} else {
				// Clears existing diagnostics.
				self.client.publish_diagnostics(uri.clone(), Vec::new(), Some(version)).await;
			}
		} else {
			self.client.log_message(MessageType::ERROR, format!("invalid document URI: {uri}")).await;
		}
	}

	fn file_for_uri(&self, uri: &Url) -> Result<Arc<RwLock<AssemblyFile>>> {
		let path = &uri.to_file_path().map_err(|()| tower_lsp::jsonrpc::Error {
			code:    tower_lsp::jsonrpc::ErrorCode::InvalidParams,
			data:    None,
			message: format!("invalid document URI {uri}"),
		})?;
		self.environment.read().files.get::<Path>(path).cloned().ok_or_else(|| tower_lsp::jsonrpc::Error {
			code:    tower_lsp::jsonrpc::ErrorCode::InvalidParams,
			data:    None,
			message: format!("no parsed document found for {uri}"),
		})
	}
}

#[tokio::main]
async fn main() {
	env_logger::init();

	let stdin = tokio::io::stdin();
	let stdout = tokio::io::stdout();

	let environment = Environment::new();
	let frontend = Arc::new(ServerFrontend::default());
	environment.write().set_error_options(frontend.clone());
	let (service, socket) = LspService::build(|client| Backend { client, frontend, environment }).finish();

	Server::new(stdin, stdout, socket).serve(service).await;
}
