exports._retrieveJQuery = page => () =>
  page.evaluate(() => window.fetch('https://cdnjs.cloudflare.com/ajax/libs/jquery/3.6.0/jquery.min.js').then((res) => res.text()));

exports._typeInto = selector => text => page => () =>
    page.focus(selector).then(() => page.keyboard.type(text));

exports._clickTab = chrome => walletPage => () => {
    walletPage.evaluate(() => {
	chrome.tabs.query({ active: true }, tabs => {
	    chrome.browserAction.onClicked.dispatch(tabs[0]);
	})
    })
}
