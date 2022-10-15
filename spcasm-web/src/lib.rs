//! Interactive in-browser playground for ``spcasm``, powered by `WebAssembly`.
#![deny(clippy::all, clippy::pedantic, clippy::nursery)]
#![allow(non_upper_case_globals, clippy::default_trait_access)]

use std::sync::Arc;

use html_escape::{decode_html_entities, encode_safe};
use miette::{GraphicalReportHandler, GraphicalTheme};
use once_cell::sync::Lazy;
use regex::{Captures, Regex};
use spcasm::{pretty_hex, run_assembler_on_source, AssemblyCode, AssemblyError};
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::HtmlElement;

#[allow(unused)]
macro_rules! log {
	( $( $t:tt )* ) => {
		web_sys::console::log_1(&format!( $( $t )* ).into())
	}
}

#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

static NEWLINE_EQUIVALENT: Lazy<Regex> = Lazy::new(|| Regex::new(r"<br(/)?>").unwrap());
static ANSI_CSI_ESCAPE: Lazy<Regex> = Lazy::new(|| Regex::new(r"\[(.+?)m").unwrap());

static REPORT_HANDLER: Lazy<GraphicalReportHandler> = Lazy::new(|| {
	GraphicalReportHandler::new_themed(GraphicalTheme::unicode())
		.with_cause_chain()
		.with_context_lines(3)
		.tab_width(4)
		.with_links(false)
});

fn htmlify(text: &str) -> String {
	ansi_to_html(&encode_safe(text).replace(' ', "&nbsp;")).replace('\n', "<br/>")
}

fn ansi_to_html(text: &str) -> String {
	// FIXME: This is a crude converter that only handles the escapes the renderer actually uses.
	let text = text.replace("[2m", "<span class=\"ansi-faint\">").replace("[0m", "</span>");

	ANSI_CSI_ESCAPE
		.replace_all(&text, |captures: &Captures| {
			let csi_command = &captures[1];
			let mut numbers =
				csi_command.split(';').filter(|string| !string.is_empty()).map(|string| string.parse::<u64>().unwrap());

			let mut replacement_text = "<span".to_string();

			while let Some(number) = numbers.next() {
				// Again, we only implement what we need here.
				match number {
					0 => replacement_text.push_str(" class=\"ansi-reset\""),
					1 => replacement_text.push_str(" class=\"ansi-bold\""),
					2 => replacement_text.push_str(" class=\"ansi-faint\""),
					3 => replacement_text.push_str(" class=\"ansi-italic\""),
					4 => replacement_text.push_str(" class=\"ansi-underline\""),
					// Set foreground color
					38 => {
						let two = numbers.next().unwrap();
						assert!(two == 2, "5;n not supported");
						let r = numbers.next().unwrap();
						let g = numbers.next().unwrap();
						let b = numbers.next().unwrap();
						replacement_text.push_str(&format!(" style=\"color:rgb({r},{g},{b})\""));
					},
					code => unimplemented!("csi code {}", code),
				}
			}
			replacement_text.push('>');

			replacement_text
		})
		.to_string()
}

#[wasm_bindgen(module = "/www/lib.js")]
extern "C" {
	#[wasm_bindgen(js_name = "outputWidth")]
	fn output_width() -> f64;
}

fn set_panic_hook() {
	#[cfg(feature = "console_error_panic_hook")]
	console_error_panic_hook::set_once();
}

/// Callback for when the user changes assembly code.
#[allow(clippy::cast_sign_loss, clippy::cast_possible_truncation)]
#[wasm_bindgen]
pub fn on_assembly_change() {
	set_panic_hook();

	let document = web_sys::window().unwrap().document().unwrap();
	let code_input = document.query_selector("code.assembly-source").unwrap().unwrap();
	let output = document.query_selector("#output").unwrap().unwrap();
	let status_paragraph = document.query_selector("p#status").unwrap().unwrap();

	let output_width = output_width() as usize;
	let report_handler = REPORT_HANDLER.clone().with_width(output_width);

	// Replace the divs that the browser inserts with newlines
	let code_text = code_input.unchecked_into::<HtmlElement>().inner_text();
	let code_text = NEWLINE_EQUIVALENT.replace_all(&code_text, "\n");
	// Decode other entities
	let code_text = decode_html_entities(&code_text);

	let source = Arc::new(AssemblyCode::new(&code_text, "<<input>>".to_owned()));

	// TODO: Allow the user to specify error options?
	let start_time = js_sys::Date::now();
	let assembler_result = run_assembler_on_source(&source, &Default::default());

	let end_time = js_sys::Date::now();
	let elapsed_time = end_time - start_time;

	let status_text = match assembler_result {
		Ok((_environment, binary)) => {
			let binary_text = pretty_hex(&binary);
			output.set_inner_html(&htmlify(&binary_text));
			"Assembly compiled successfully."
		},
		Err(why) => {
			let mut rendered = String::new();
			let why = why.downcast::<AssemblyError>().expect("no AssemblyError found");
			report_handler.render_report(&mut rendered, &why).expect("couldn't render report");
			output.set_inner_html(&htmlify(&rendered));
			"Couldn't compile assembly."
		},
	};
	status_paragraph.set_inner_html(&htmlify(&format!("{status_text} ({}ms)", elapsed_time.floor() as i64)));
}
