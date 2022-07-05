exports._retrieveJQuery = page => () =>
  page.evaluate(() => window.fetch('https://cdnjs.cloudflare.com/ajax/libs/jquery/3.6.0/jquery.min.js').then((res) => res.text()));


exports._text = msg => () => msg ? msg.text() : "";
exports._consoleMessageText = function(consoleMessage) {
    return "XY";
    return function() {
	return "YOYOBA";
      let ret = consoleMessage.text();
      if(ret)
	 return ret;
      else
	 return "";
   }
}
