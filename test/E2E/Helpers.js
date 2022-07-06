exports._retrieveJQuery = page => () =>
  page.evaluate(() => window.fetch('https://cdnjs.cloudflare.com/ajax/libs/jquery/3.6.0/jquery.min.js').then((res) => res.text()));

exports._typeInto = selector => text => page => () =>
    page.focus(selector).then(() => page.keyboard.type(text));

exports._clickTab = chrome => walletPage => () => {
    console.log("A");
    walletPage.evaluate(() => {
    console.log("B");	
	chrome.tabs.query({ active: true }, tabs => {
    console.log("C");	    
	    chrome.browserAction.onClicked.dispatch(tabs[0]);
	})
    })
}

exports._openPopup = browser => {
    async function f() {
	// find extension service worker and get it
	const extBackgroundTarget = await browser.waitForTarget(t => t.type() === 'service_worker');
	const extWorker = await extBackgroundTarget.worker();

	// load a page from which to open the extension popup, make it the active tab
	const someOtherPage = await browser.newPage();
	await someOtherPage.goto("https://www.google.com", { waitUntil: ['domcontentloaded', "networkidle2"] });
	await someOtherPage.bringToFront();

	await extWorker.evaluate(() => {
	    chrome.action.openPopup(); // Add arguments as needed
	});
    }
    return f;
}
