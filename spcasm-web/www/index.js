import * as spcasm from "spcasm-web-wasm";

function cleanDOMStructure(event) {
  const target = event.target;
  const text = target.innerText;
  // If there are div child elements, user most likely copy-pasted and we need to clean the DOM.
  if (target.querySelectorAll("div").length > 0) target.innerHTML = text;
}

const sourceInput = document.querySelector("code.assembly-source");
sourceInput.addEventListener("input", spcasm.on_assembly_change, false);
sourceInput.addEventListener("input", cleanDOMStructure, false);

spcasm.on_assembly_change();
