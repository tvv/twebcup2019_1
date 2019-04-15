'use strict';

import App from '../elm/Main.elm';

(function (fn) {
  fn(window.jQuery, window, document);
}(function ($, window, document) {

  let
    context = {
      $: $,
      window: window,
      document: document,
      app: undefined
    };

  $(() => {
    const app = App.Elm.Main.init({flags: {}});

    context.app = app;
  });

}));
