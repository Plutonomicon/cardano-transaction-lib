async function findAndClickButton (browser, jQuery, text) {

    console.log("Try to click " + text);
    pages = await browser.pages();
    console.log(pages.length + " pages:");

    for(i=0;i<pages.length;i++) {
	j = i;

	await pages[j].evaluate(() => console.log("jquery"));
	await pages[j].evaluate(jQuery);
	await pages[j].evaluate(() => console.log("done"));
	
	
	prefix = "P"+JSON.parse(JSON.stringify(j));
	
	pages[j]
	    .on('console', message =>
		console.log(`${prefix} ${message.type().substr(0, 3).toUpperCase()} ${message.text()}`))
	    .on('pageerror', ({ message }) => console.log(message))
	    .on('response', response =>
		console.log(`${response.status()} ${response.url()}`))
	    .on('requestfailed', request =>
		console.log(`${request.failure().errorText} ${request.url()}`));
	
	console.log("eval in page " + j);
	
	await pages[i].evaluate((j, text) => {	    
	    
	    console.log("hello from page "+j);
	    
	    console.log("Body length " + document.body.children.length);
	    console.log("Head length " + document.head.children.length);

/*	    var walker = document.createTreeWalker(
		document.body,
		NodeFilter.SHOW_ELEMENT // only elements
	    );
	    while (walker.nextNode()) {
		let current = walker.currentNode;
		console.log(
		    current.tagName,
		    [...current.attributes].map(({value,name}) => `${name}=${value}`).join()
		);
	    }
*/
	    buttons = document.querySelectorAll("button");

	    console.log("Number of buttons: " + buttons.length);

	    for(g = 0; g<buttons.length; g++) {
		console.log(g+" value "+buttons[g].value);
		console.log(g+" innerText "+buttons[g].innerText);
		if(buttons[g].innerText == text) {
		    
		    buttons[g].click()
		}
	    }

	    jqButtons = console.log("jQuery('button')");
	    console.log("jQButton: " + jQuery('button'));
	    
	    jQuery(`button:contains(${text})`).click();
	    
	    /*
	      let observer = new MutationObserver ((mutations) => {
	      console.log("Mutations: " + mutations);
	      });
	      
	      observer.observe(document.body, {
	      childList : true
	      , subTree : true
	      , attributes: true
	      , characterData: true
	      });

	      console.log("observing");*/
	    /*	    
		    buttons = document.querySelectorAll("button");
		    while(buttons.length == 0) {
		    buttons = document.querySelectorAll("button");
		    }
		    buttons.forEach(b => { 
		    console.log(" " + b.nodeName);
		    b.childNodes.forEach(c =>
		    console.log(" " + c.nodeName))
		    }); */
	}, j, text);
    }
}

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
		, `--user-data-dir=chrome-data`
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


	/* works
	const jquery = await page.evaluate(() => window.fetch('https://cdnjs.cloudflare.com/ajax/libs/jquery/3.6.0/jquery.min.js').then((res) => res.text()));
	*/

	/* works
	const ffetch = () => window.fetch('https://cdnjs.cloudflare.com/ajax/libs/jquery/3.6.0/jquery.min.js').then((res) => res.text());
	
	const jquery = await page.evaluate(ffetch);	
	*/

	const fetch ="async function x() { return window.fetch('https://cdnjs.cloudflare.com/ajax/libs/jquery/3.6.0/jquery.min.js').then((res) => res.text()); } await x();";
	
	const jquery = await page.evaluate(fetch);
	
//	const newPagePromise = new Promise(x => page.once('popup', x));
	console.log('C1');	
	page.goto(example);
	console.log('C2');

	pages = await browser.pages();

	newPage = pages[0];
	
	console.log('C3');
	console.log('D');

//	await newPage.waitForSelector("button");
	
	console.log('E');

	try { await page.waitForNavigation({timeout:2000}); } catch (e) {}

	findAndClickButton(browser, jquery, "Access");

	try { await page.waitForNavigation({timeout:2000}); } catch (e) {}
	
	findAndClickButton(browser, jquery, "Sign");	
	

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
