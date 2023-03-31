"use strict";

// This needs to be asynchronous to load the WASM from CSL
//
// You also need to call `spago bundle-module` to generate the module that is
// imported here. From the repository root, run:
//   spago bundle-module -m <MAIN> --to output.js
import("../output.js").then(m => m.main());

// Set up Blockfrost API key prompt and UI around it.
// This code is only needed for CTL itself.
const oldBlockfrostKey = localStorage.getItem("BLOCKFROST_API_KEY");

const addSpacer = () => {
  const spacer = document.createElement("span");
  spacer.textContent = " ";
  document.body.appendChild(spacer);
};

const addHtml = html => {
  const div = document.createElement("div");
  div.innerHTML = html;
  document.body.appendChild(div);
};

addHtml(`<h3>Blockfrost setup</h3>`);

if (oldBlockfrostKey !== null) {
  const keyIsSetEl = document.createElement("span");
  keyIsSetEl.innerHTML =
    '✅ <a target="_blank" href="https://blockfrost.io/">Blockfrost</a> key is set.';
  document.body.appendChild(keyIsSetEl);

  addSpacer();

  const setBFKeyButton = document.createElement("button");
  setBFKeyButton.textContent = "Unset";
  setBFKeyButton.onclick = () => {
    localStorage.removeItem("BLOCKFROST_API_KEY");
    window.location = "/";
  };
  document.body.appendChild(setBFKeyButton);
} else {
  const keyIsNotSetEl = document.createElement("span");
  keyIsNotSetEl.innerHTML =
    '❌ <a target="_blank" href="https://blockfrost.io/">Blockfrost</a> key is not set.';
  document.body.appendChild(keyIsNotSetEl);

  addSpacer();

  const setBFKeyButton = document.createElement("button");
  setBFKeyButton.textContent = "Set Blockfrost API key";
  setBFKeyButton.onclick = () => {
    const key = prompt("Enter your Blockfrost API key");
    if (key !== null) {
      localStorage.setItem("BLOCKFROST_API_KEY", key);
      window.location = "/";
    }
  };
  document.body.appendChild(setBFKeyButton);
}

addHtml(`<br>To set the Blockfrost API key permanently, close the browser and run:
  <pre>
    npm run e2e-pack-settings
  </pre>
  The key will be saved to your settings archive. Sharing the archive file after that implies that the key can be leaked.<br>`);
