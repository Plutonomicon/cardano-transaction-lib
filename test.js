

async function test() {

    const namiHash = 'lpfcbjknijpeeillifnkikgncikgfhdo';
    const extensionPath = `/home/mike/.config/google-chrome/Default/Extensions/${namiHash}/3.2.5_1`;
    const welcomePage = `chrome-extension://${namiHash}/index.html`;

    const otherPage = 'chrome-extension://lpfcbjknijpeeillifnkikgncikgfhdo/mainPopup.bundle.js';

    const normalPage = "http://www.orf.at/"
    const example = "http://localhost:4008/"
    
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
//		, '--user-data-dir="/tmp/ChromeProfile"'
		//	      ,	`--headless=chrome`
	      ]
	    }
	);

/*	const backgroundPageTarget = await browser.waitForTarget(
	    (target) => target.type() === 'background_page'
	);
	const backgroundPage = await backgroundPageTarget.page();
********/	
	console.log('B');
	const page = await browser.newPage();

	console.log('C');
	await page.goto(example);

	const nami = await page.evaluate(() => {
	    console.log("X");
	    console.log(window);
	    console.log("Y");
	    console.log(cardano);
	    console.log("Z");	    
	    //cardano.nami.enable()
	});
	
	console.log('E');
//	await browser.close();

    }
    catch(err) {
	console.error(err);
    }
}

test();
