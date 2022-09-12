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
