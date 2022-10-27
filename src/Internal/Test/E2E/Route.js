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
  const table = document.createElement("table");
  table.border = "1";
  const header = document.createElement("tr");

  header.appendChild(document.createElement("th"));
  for (let config of configs) {
    const td = document.createElement("th");
    td.textContent = config;
    header.appendChild(td);
  }
  table.appendChild(header);

  for (let test of tests) {
    const tr = document.createElement("tr");
    const titleTd = document.createElement("td");
    titleTd.textContent = test;
    tr.appendChild(titleTd);
    for (let config of configs) {
      const td = document.createElement("td");
      const link = document.createElement("a");
      link.href = "?" + config + ":" + test;
      link.textContent = config;
      td.appendChild(link);
      tr.appendChild(td);
    }
    table.appendChild(tr);
  }
  document.body.appendChild(table);
};
