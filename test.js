

async function test() {

    const namiHash = 'lpfcbjknijpeeillifnkikgncikgfhdo';
    const extensionPath = `/home/mike/.config/google-chrome/Default/Extensions/${namiHash}/3.2.5_1`;
    const welcomePage = `chrome-extension://${namiHash}/index.html`;

    const otherPage = 'chrome-extension://lpfcbjknijpeeillifnkikgncikgfhdo/mainPopup.bundle.js';
    const jsPage = 'chrome-extension://lpfcbjknijpeeillifnkikgncikgfhdo/injected.bundle.js';
    const normalPage = "http://www.orf.at/";
    const example = "http://localhost:4008/";

    const namiStorePage = "https://chrome.google.com/webstore/detail/nami/lpfcbjknijpeeillifnkikgncikgfhdo?hl=en";
    
    const puppeteer = require('puppeteer');

    try {
	console.log('A');
	console.log(extensionPath);
	let browser = await puppeteer.launch(
	    { headless: false
	      , executablePath: '/usr/bin/google-chrome'
	      ,args:
	      [ `--disable-extensions-except=${extensionPath}`
		, `--load-extension=${extensionPath}`
		, '--user-data-dir="/tmp/ChromeProfile"'
//		, `--headless=chrome`
	      ]
	    }
	);
/*
	console.log(browser.browserContexts());
	
	console.log("1");
	const target = await new Promise(resolve => {
	    var targets = browser.targets();
	    console.log(targets);
	    const target = targets.find(target => target.type() === 'background_page' && target.url().endsWith('_generated_background_page.html'));
	    if (target)
	    {
		console.log("target: "+target);
		return resolve(target);
	    }
	    
	    const listener = target => {
		if (target.type() === 'background_page' && target.url().endsWith('_generated_background_page.html')) {
		    browser.removeListener('targetcreated', listener);
		    browser.removeListener('targetchanged', listener);
		    resolve(target);
		}
	    };
	    console.log("listen...");
	    browser.on('targetcreated', listener);
	    browser.on('targetchanged', listener);
	});
	
	console.log("2");	
	var backgroundPage = await target.page();
	console.log("3");	
	await backgroundPage.reload();
	console.log("4");
    */
	/*	const backgroundPageTarget = await browser.waitForTarget(
		(target) => target.type() === 'background_page'
		);
		const page = await backgroundPageTarget.page();
	*/
	//	console.log('B');
	const page = await browser.newPage();

	page
	    .on('console', message =>
		console.log(`${message.type().substr(0, 3).toUpperCase()} ${message.text()}`))
	    .on('pageerror', ({ message }) => console.log(message))
	    .on('response', response =>
		console.log(`${response.status()} ${response.url()}`))
	    .on('requestfailed', request =>
		console.log(`${request.failure().errorText} ${request.url()}`));

	
	console.log('C');

//	const newPagePromise = new Promise(x => page.once('popup', x));
	console.log('C1');	
	page.goto(example);
	console.log('C2');

	console.log(await browser.pages()[0]);
	
/*	const newPage = await newPagePromise;
	console.log('C3');
	newPage
	    .on('console', message =>
		console.log(`NP: ${message.type().substr(0, 3).toUpperCase()} ${message.text()}`))
	    .on('pageerror', ({ message }) => console.log(message))
	    .on('response', response =>
		console.log(`${response.status()} ${response.url()}`))
	    .on('requestfailed', request =>
		console.log(`${request.failure().errorText} ${request.url()}`));
*/	
	console.log('D');
	
//	button = await page.$x("//div[contains(., 'Add to Chrome')]");

//	console.log(button);
	
/*	
	await page.evaluate(() => {
	    const jsPage = 'chrome-extension://lpfcbjknijpeeillifnkikgncikgfhdo/injected.bundle.js';	    
	    document.write(`
<html>
  <head><script src="${jsPage}" type="text/javascript"></script>
  </head>
  <body></body>
</html>`);
	});
	await page.evaluate(() => {					 
	    console.log("X");
	    console.log(window);
	    console.log("Y");
	    console.log(window.cardano);
	    console.log(cardano);	    	    
	    console.log("Z");	    
	    //cardano.nami.enable()
	});
	console.log('E');
	await browser.close();
*/

    }
    catch(err) {
	console.error(err);
    }
}

test();
