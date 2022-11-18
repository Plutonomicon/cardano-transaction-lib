exports._retrieveJQuery = page => () =>
  page.evaluate(() =>
    window
      .fetch(
        "https://cdnjs.cloudflare.com/ajax/libs/jquery/3.6.0/jquery.min.js"
      )
      .then(res => res.text())
      .catch(_ => "")
  );

// try {
//   Error.stackTraceLimit = Infinity;
// } catch (_) {}

exports._ourGoto = page => url => () => {
  return page.goto(url, { waitUntil: "networkidle0" });
};
