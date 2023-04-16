"use strict";

import * as spcasm from "spcasm-web";

// Creates an options object from the UI input.
function createOptions() {
  return {
    silence_all: false,
    silenced: [],
    max_reference_resolution_passes: Number(
      document.querySelector("#max-reference-resolution-passes").value
    ),
    max_macro_expansion_depth: Number(
      document.querySelector("#max-macro-expansion-depth").value
    ),
  };
}

const updatingObjects = [
  document.querySelector("code.assembly-source"),
  document.querySelector("#max-reference-resolution-passes"),
  document.querySelector("#max-macro-expansion-depth"),
];

for (const object of updatingObjects) {
  object.addEventListener(
    "input",
    () => spcasm.on_assembly_change(createOptions()),
    false
  );
}

spcasm.on_assembly_change(createOptions());
