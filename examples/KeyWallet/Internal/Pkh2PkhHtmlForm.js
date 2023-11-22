"use strict";

const form = `
<style>
form {
  max-width: 1000px;
}

fieldset {
  display: grid;
  padding: 10px;
  grid-row-gap: 10px;
  grid-template-columns: 25% 75%;
}

button {
  float: right;
}

code {
  display: block;
  font-family: Consolas,"courier new";
  padding: 2px;
  font-size: 100%;
}
</style>
<form>
 <fieldset>
    <label for="privateKey">Private key (hex):</label>
    <input type="password" id="private_key" name="privateKey" placeholder="8f093aa4103fb26121148fd2ece4dd1d775be9113dfa374bcb4817b36356180b">
    <label for="toPkh">Destination PKH (bech32):</label>
    <input type="text" id="to_pkh" name="toPkh" placeholder="addr_vkh14lk6mlsm50ewtn9p5zgd7lkfdalsycdcxjpl5s979st9xjdmdc4">
    <label for="lovelace">Amount (lovelace):</label>
    <input type="number" min="0" id="lovelace" name="lovelace" placeholder="2000000" value="2000000">
    <div style="grid-column: 2;">
      <button type="submit">Submit</button>
    </div>
 </fieldset>
</form>
<code></code>
`;

export function logError(error) {
  return () => {
    console.log(error);
  };
}

export function mkForm(handler) {
  return () => {
    window.document.body.insertAdjacentHTML("beforeend", form);
    const formEl = window.document.querySelector("form");
    const fieldsEl = window.document.querySelector("fieldset");
    const resultEl = window.document.querySelector("code");
    formEl.addEventListener("submit", event => {
      event.preventDefault();
      resultEl.replaceChildren();

      const data = new FormData(formEl);
      const input = Object.fromEntries(data);
      fieldsEl.setAttribute("disabled", "disabled");

      const log = color => text => () => {
        const line = document.createElement("div");
        line.style.color = color;
        line.textContent = text;
        resultEl.append(line);
      };

      const unlock = () => {
        fieldsEl.setAttribute("disabled", "disabled");
        fieldsEl.removeAttribute("disabled");
      };

      handler(input)(log)(unlock)();
    });
  };
}
