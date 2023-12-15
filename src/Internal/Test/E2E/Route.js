export function _queryString() {
  return window.location.search;
}

export function _setupBlockfrostApi() {
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

  addHtml(`<br>If you are viewing this page via an E2E-test-suite-controlled browser, to set the Blockfrost API key permanently, close the browser and run:
      <pre>
        npm run e2e-pack-settings
      </pre>
      The key will be saved to your settings archive. Sharing the archive file after that implies that the key can be leaked.<br>`);
}

const createLink = (example, wallet) =>
  '<a href="/?' + example + ":" + wallet + '">' + wallet + "</a>";

export function _writeExampleHTML(example) {
  return wallets => () => {
    const ul = document.getElementById("examples");
    const li = document.createElement("li");
    li.innerHTML = `${example}: ${wallets
      .map(w => createLink(example, w))
      .join(" ")}`;
    ul.appendChild(li);
  };
}

export function _addLinks(configs) {
  return tests => () => {
    const configSelectEl = document.createElement("select");
    const testNameSelectEl = document.createElement("select");
    const linkEl = document.createElement("a");
    linkEl.textContent = "➡ RUN EXAMPLE";

    const h1El = document.createElement("h3");
    h1El.textContent = "Example runner";

    const selectEnvironmentEl = document.createElement("span");
    selectEnvironmentEl.textContent = "Environment: ";

    const selectExampleEl = document.createElement("span");
    selectExampleEl.textContent = "Example: ";

    document.body.appendChild(h1El);
    document.body.appendChild(selectEnvironmentEl);

    const updateUrl = () => {
      linkEl.href = "?" + configSelectEl.value + ":" + testNameSelectEl.value;
    };

    configs.forEach((config, ix) => {
      const optionEl = document.createElement("option");
      optionEl.textContent = config;
      optionEl.value = config;
      configSelectEl.appendChild(optionEl);
      if (document.location.search.startsWith("?" + config + ":")) {
        configSelectEl.selectedIndex = ix;
      }
    });
    document.body.appendChild(configSelectEl);
    document.body.appendChild(document.createElement("br"));

    document.body.appendChild(selectExampleEl);

    tests.forEach((test, ix) => {
      const optionEl = document.createElement("option");
      optionEl.textContent = test;
      optionEl.value = test;
      testNameSelectEl.appendChild(optionEl);
      if (document.location.search.endsWith(":" + test)) {
        testNameSelectEl.selectedIndex = ix;
      }
    });
    document.body.appendChild(testNameSelectEl);

    updateUrl();
    document.body.appendChild(document.createElement("br"));

    document.body.appendChild(linkEl);

    configSelectEl.onchange = updateUrl;
    testNameSelectEl.onchange = updateUrl;
  };
}
