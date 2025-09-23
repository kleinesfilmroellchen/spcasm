//! Interactive in-browser playground for ``spcasm``, powered by `WebAssembly`.
#![allow(clippy::default_trait_access)]

use std::fmt::Write;
use std::sync::{Arc, LazyLock};

use flexstr::SharedStr;
use html_escape::{decode_html_entities, encode_safe};
use miette::{GraphicalReportHandler, GraphicalTheme};
use options::WebOptions;
use regex::{Captures, Regex};
use spcasm::{AssemblyCode, pretty_hex, run_assembler};
use wasm_bindgen::JsCast;
use wasm_bindgen::prelude::*;
use web_sys::HtmlElement;

mod options;

/// Log to web console.
#[allow(unused)]
#[macro_export]
macro_rules! log {
	( $( $t:tt )* ) => {
		web_sys::console::log_1(&format!( $( $t )* ).into())
	}
}

#[cfg(target_family = "wasm")]
#[global_allocator]
static ALLOCATOR: talc::Talck<parking_lot::RawMutex, talc::WasmHandler> =
	talc::Talc::new(unsafe { talc::WasmHandler::new() }).lock();

static NEWLINE_EQUIVALENT: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"<br(/)?>").unwrap());
static ANSI_CSI_ESCAPE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\[(.+?)m").unwrap());

static REPORT_HANDLER: LazyLock<GraphicalReportHandler> = LazyLock::new(|| {
	GraphicalReportHandler::new_themed(GraphicalTheme::unicode())
		.with_cause_chain()
		.with_context_lines(3)
		.tab_width(4)
		.with_links(false)
});

fn htmlify(text: &str) -> SharedStr {
	ansi_to_html(&encode_safe(text).replace(' ', "&nbsp;")).replace('\n', "<br/>").into()
}

fn ansi_to_html(text: &str) -> SharedStr {
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
					30 => replacement_text.push_str(" class=\"ansi-fg-black\""),
					31 => replacement_text.push_str(" class=\"ansi-fg-red\""),
					32 => replacement_text.push_str(" class=\"ansi-fg-green\""),
					33 => replacement_text.push_str(" class=\"ansi-fg-yellow\""),
					34 => replacement_text.push_str(" class=\"ansi-fg-blue\""),
					35 => replacement_text.push_str(" class=\"ansi-fg-magenta\""),
					36 => replacement_text.push_str(" class=\"ansi-fg-cyan\""),
					37 => replacement_text.push_str(" class=\"ansi-fg-white\""),
					38 => {
						let two = numbers.next().unwrap();
						assert!(two == 2, "5;n not supported");
						let r = numbers.next().unwrap();
						let g = numbers.next().unwrap();
						let b = numbers.next().unwrap();
						write!(replacement_text, " style=\"color:rgb({r},{g},{b})\"").unwrap();
					},
					code => unimplemented!("csi code {}", code),
				}
			}
			replacement_text.push('>');

			replacement_text
		})
		.as_ref()
		.into()
}

#[wasm_bindgen(module = "/lib.js")]
extern "C" {
	#[wasm_bindgen(js_name = "outputWidth")]
	fn output_width() -> f64;
}

#[allow(clippy::missing_const_for_fn)]
fn set_panic_hook() {
	#[cfg(feature = "console_error_panic_hook")]
	console_error_panic_hook::set_once();
}

/// For build system reasons, we're compiled as a binary crate, so we need an (unused) main function.
#[allow(dead_code)]
const fn main() {}

/// Callback for when the user changes assembly code.
///
/// # Panics
/// Broken DOM and JavaScript weirdness.
#[allow(clippy::cast_sign_loss, clippy::cast_possible_truncation)]
#[wasm_bindgen]
pub fn on_assembly_change(options: JsValue) {
	set_panic_hook();

	let options: Arc<WebOptions> = Arc::new(serde_wasm_bindgen::from_value(options).unwrap());

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

	let source = Arc::new(AssemblyCode::new(&code_text, "<<input>>"));

	let start_time = js_sys::Date::now();
	let assembler_result = run_assembler(&source, options.clone());

	let end_time = js_sys::Date::now();
	let elapsed_time = end_time - start_time;

	let status_text = if let Ok((_environment, binary)) = assembler_result {
		let binary_text = pretty_hex(&binary, None);
		output.set_inner_html(&htmlify(&binary_text));
		"Assembly compiled successfully."
	} else {
		let mut rendered = String::new();
		#[allow(clippy::significant_drop_in_scrutinee)] // noone else can actually access the frontend object
		for error in options.diagnostics.read().unwrap().iter() {
			report_handler.render_report(&mut rendered, error).expect("couldn't render report");
		}
		output.set_inner_html(&htmlify(&rendered));
		"Couldn't compile assembly."
	};
	status_paragraph.set_inner_html(&htmlify(&format!("{status_text} ({}ms)", elapsed_time.floor() as i64)));
}
