// JavaScript support functions for Rust

// Approximates the width of the output box in characters by hijacking text measuring APIs from canvas.
export function outputWidth() {
  const canvas =
    outputWidth.canvas ||
    (outputWidth.canvas = document.createElement("canvas"));
  const context = canvas.getContext("2d");
  context.font = "100% 'Cascadia Code', 'Source Code Pro', monospace";
  // To account for inter-character spacing and kerning, measure a bunch of characters
  const repeat = 80;
  const metrics = context.measureText("M".repeat(repeat));
  // The calculation is off by almost 30%, I have no idea why but this seems to work.
  return (
    (document.querySelector("#output").clientWidth / (metrics.width / repeat)) *
    0.7
  );
}
