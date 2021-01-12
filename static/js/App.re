open ReactIntl;
open Navigator;

[@react.component]
let make = () => {
  let chooseLocale = () => {
    switch (Js_string.split("-", Language.make)[0]) {
    | "zh" => Locale.Zh
    | _ => Locale.En
    };
  };

  <IntlProvider
    locale={chooseLocale()->Locale.toString}
    messages={chooseLocale()->Locale.translations->Translation.toDict}>
    <Route />
  </IntlProvider>;
};
