exports._queryString = () => window.location.search;

const createLink = (example, wallet) =>
  '<a href="/?' + example + ":" + wallet + '">' + wallet + "</a>";

exports._writeExampleHTML = example => wallets => () => {
  const ul = document.getElementById("examples");
  const li = document.createElement("li");
  li.innerHTML = `${example}: ${wallets
    .map(w => createLink(example, w))
    .join(" ")}`;
  ul.appendChild(li);
};

exports._addLinks = configs => tests => () => {
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
