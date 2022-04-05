import('cardano-transaction-lib-seabug').then(
    async seabug => {
        document.body.textContent += await seabug.callMarketPlaceBuyTest('hi from JS');
    }
);
