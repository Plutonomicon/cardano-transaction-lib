export function _retrieveJQuery(page) {
  return () =>
    page.evaluate(() =>
      window
        .fetch(
          "https://cdnjs.cloudflare.com/ajax/libs/jquery/3.6.0/jquery.min.js"
        )
        .then(res => res.text())
        .catch(_ => "")
    );
}
