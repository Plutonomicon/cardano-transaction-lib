export function _typeInto(selector) {
  return text => page => () =>
    page.focus(selector).then(() => page.keyboard.type(text));
}

export function pageUrl(page) {
  return () => page.url();
}
