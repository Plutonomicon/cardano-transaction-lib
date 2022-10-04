"use strict";

const mkRow = id => {
  var template = document.createElement("template");
  template.innerHTML =
    `<tr class="table-warning">
  <td><samp>${id}</samp></td>
  <td>
  <button class="btn btn-primary btn-lg" type="button" disabled>
    <span class="sending">
      <span class="spinner-grow spinner-grow-sm" role="status" aria-hidden="true"></span>
    </span>
    <span class="retrieve" hidden="hidden" style="width: 1rem; height: 1rem; display: inline-block">↑</span>
    <span class="retrieving" hidden="hidden">
      <span class="spinner-grow spinner-grow-sm" role="status" aria-hidden="true"></span>
    </span>
  </button>
  </td>
  </td>
</tr><tr class="table-warning"><td colspan="2" class="text-center">pending</td></tr>`;
  return {
    main: template.content.childNodes[0],
    info: template.content.childNodes[1],
  };
};

const mkToast = (id, direction, amount, fee) => {
  var to, from;

  if (direction == "TO_WALLET") {
    to = "wallet";
    from = "script";
  } else {
    to = "script";
    from = "wallet";
  }

  var template = document.createElement("template");
  template.innerHTML =
    `<div class="toast fade">
  <div class="toast-header">
      <strong class="me-auto" style="white-space: nowrap; text-overflow: ellipsis; overflow: hidden;">
        Tx Complete: ${id}
      </strong>
      <button type="button" class="btn-close" data-bs-dismiss="toast"></button>
  </div>
  <div class="toast-body">
    <div class="d-flex">
      <span class="me-auto">${from} → ${to}</span>
      ${direction == "FROM_WALLET" ? "-" : ""}${amount} Ada
    </div>
    <div class="d-flex justify-content-end">
      ${Number(fee).toFixed(2)} Ada fee
    </div>
  </div>
</div>`;
  return template.content.childNodes[0];
};

exports._setBalance = bal => () => {
  const balanceEl = document.querySelector("#balance");
  balanceEl.textContent = bal;
};

exports._onSend = cont => () => {
  const formEl = document.querySelector("form");

  const sendButton = document.querySelector("#send");
  const sendEl = document.querySelector("#form-button-send");
  const sendingEl = document.querySelector("#form-button-sending");
  const statusEl = document.querySelector("#form-status");
  const toaster = document.querySelector("#toaster");

  const rows = document.querySelector("tbody");

  formEl.addEventListener("submit", event => {
    event.preventDefault();

    var currentStatusEl = statusEl;
    function setStatus(status) {
      return function () {
        currentStatusEl.innerHTML = status;
      };
    }

    sendButton.disabled = true;
    sendEl.hidden = true;
    sendingEl.hidden = false;

    const data = new FormData(formEl);
    const input = Object.fromEntries(data);

    const onBuilt = txid => fee => cont => () => {
      sendButton.disabled = false;
      sendEl.hidden = false;
      sendingEl.hidden = true;

      const row = mkRow(txid);
      const rowSendingEl = row.main.querySelector(".sending");
      const rowRetrieveEl = row.main.querySelector(".retrieve");
      const rowRetrievingEl = row.main.querySelector(".retrieving");
      const rowRetrieveButton = row.main.querySelector("button");
      const rowInfoEl = row.info.querySelector("td");

      rowInfoEl.innerHTML = currentStatusEl.innerHTML;
      currentStatusEl.innerHTML = "";
      currentStatusEl = rowInfoEl;

      rows.insertAdjacentElement("beforeend", row.main);
      rows.insertAdjacentElement("beforeend", row.info);

      const onRetrieve = txid => fee => () => {
        row.main.remove();
        row.info.remove();

        const toastEl = mkToast(txid, "TO_WALLET", input["ada-amount"], fee);
        const toast = toaster.insertAdjacentElement("beforeend", toastEl);
        new bootstrap.Toast(toast).show();
      };

      const onSent = cont => () => {
        const toastEl = mkToast(txid, "FROM_WALLET", input["ada-amount"], fee);
        const toast = toaster.insertAdjacentElement("beforeend", toastEl);
        new bootstrap.Toast(toast).show();

        row.main.classList.remove("table-warning");
        row.info.classList.remove("table-warning");
        row.info.hidden = true;

        rowSendingEl.hidden = true;
        rowRetrieveEl.hidden = false;
        rowRetrieveButton.disabled = false;

        rowRetrieveButton.addEventListener("click", event => {
          row.main.classList.add("table-warning");
          row.info.classList.add("table-warning");
          rowRetrieveEl.hidden = true;
          row.info.hidden = false;
          rowRetrievingEl.hidden = false;
          rowRetrieveButton.disabled = true;

          cont(onRetrieve)();
        });
      };

      cont(onSent)();
    };

    cont(input["ada-amount"])(setStatus)(onBuilt)();
  });
};
