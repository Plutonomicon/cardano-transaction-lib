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
    <label for="walletSeed">Mnemonic seed phrase:</label>
    <input id="wallet_seed" name="walletSeed" placeholder="what abstract myself forum setup leader series maximum home abuse shadow wreck inflict dust basket cycle involve quick abstract eagle staff town voyage raven">
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
