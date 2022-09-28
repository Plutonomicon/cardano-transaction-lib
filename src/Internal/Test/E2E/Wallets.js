exports._typeInto = selector => text => page => () =>
  page.focus(selector).then(() => page.keyboard.type(text));
