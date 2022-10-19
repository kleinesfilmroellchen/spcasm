import * as spcasm from "spcasm-web-wasm";

document
  .querySelector("code.assembly-source")
  .addEventListener("input", spcasm.on_assembly_change, false);

spcasm.on_assembly_change();
