open React;
open Icons;
open Data;
open Items;
open Basic;
open Status;
open Storage;
open ObjectFormat;
open IconAnimation;

type state = {
  formLoad: bool,
  error: bool,
  loading: bool,
  showYoutube: bool,
  youtubeText: string,
  disabled: bool,
  showMenu: bool,
  system: string,
  optionitems: array(optionitem),
  userid: string,
  password: string,
};

type action =
  | SettingUserId(string)
  | SettingFormItems(array(optionitem))
  | ShowMenuItem
  | ClickMenuItem(string)
  | ChangeUserId(string)
  | ChangePassword(string)
  | ActionOtherLoad(bool)
  | ActionErrorLoad(bool)
  | ActionSnackBar(string, bool);

let reducer = (state, action) =>
  switch (action) {
  | SettingUserId(value) => {
      ...state,
      userid: value,
      formLoad: !state.formLoad,
    }
  | SettingFormItems(items) => {...state, optionitems: items}
  | ShowMenuItem => {...state, showMenu: !state.showMenu}
  | ClickMenuItem(value) => {
      ...state,
      system: value,
      showMenu: !state.showMenu,
    }
  | ChangeUserId(value) => {...state, userid: value}
  | ChangePassword(value) => {...state, password: value}
  | ActionOtherLoad(other) => {...state, disabled: other, loading: other}
  | ActionErrorLoad(error) => {...state, error}
  | ActionSnackBar(youtubeText, showYoutube) => {
      ...state,
      youtubeText,
      showYoutube,
    }
  };

let initialState = {
  formLoad: false,
  error: false,
  loading: false,
  showYoutube: false,
  youtubeText: "",
  disabled: false,
  showMenu: false,
  system: "",
  optionitems: [||],
  userid: "",
  password: "",
};

let autoLoginPath = value =>
  switch (value) {
  | "" => "/"
  | _ => value
  };

[@react.component]
let make = _ => {
  let (state, dispatch) = useReducer(reducer, initialState);

  let searchAJax = () =>
    Js.Promise.(
      "newid"
      |> Locals.select
      |> userData
      |> Axiosapi.Login.search
      |> then_(response =>
           SettingFormItems(response##data##items) |> dispatch |> resolve
         )
      |> catch(error => error |> Js.log |> resolve)
      |> ignore
    );

  useEffect(() =>
    if (state.formLoad) {
      Some(() => "action" |> Js.log);
    } else {
      let testId =
        SettingUserId("userid" |> Sessions.select |> checkObjects) |> dispatch;
      let pstId =
        Navigator.Position.make(
          Location.success,
          Location.error,
          Location.items,
        )
        |> ignore;
      let searchId = searchAJax();
      //let bwsId = JsModules.Browsers.make |> ignore;
      Some(
        () => {
          testId;
          pstId;
          searchId;
        },
        //bwsId;
      );
    }
  );

  let showMenuItem = useCallback(_ => ShowMenuItem |> dispatch);

  let clickMenuItem = useCallback(value => ClickMenuItem(value) |> dispatch);

  let changeUserid =
    useCallback(value => {
      ChangeUserId(value) |> dispatch;
      value |> Sessions.create("userid");
    });

  let changePassword =
    useCallback(value => ChangePassword(value) |> dispatch);

  let restoreAction = () => {
    ActionErrorLoad(true) |> dispatch;
    Js.Global.setTimeout(
      () => {
        ActionErrorLoad(false) |> dispatch;
        ActionOtherLoad(false) |> dispatch;
      },
      500,
    )
    |> ignore;
  };

  let barShowRestoreAction = youtubeText => {
    ActionSnackBar(youtubeText, true) |> dispatch;
    Js.Global.setTimeout(() => ActionSnackBar("", false) |> dispatch, 5000)
    |> ignore;
  };

  let checkUserAJax = () =>
    Js.Promise.(
      state.userid
      |> userData
      |> Axiosapi.Login.checkUser
      |> then_(response => {
           (
             switch (response##data##status) {
             | "istrue" =>
               response##data##newid |> Sessions.create("newid");
               Path.forgetPath |> ReasonReactRouter.push;
             | _ =>
               restoreAction();
               response##data##status |> accountModule |> barShowRestoreAction;
             }
           )
           |> resolve
         })
      |> catch(error => error |> Js.log |> resolve)
      |> ignore
    );

  let forgetForm =
    useCallback(_ => {
      ActionOtherLoad(true) |> dispatch;
      checkUserAJax();
    });

  let loginUserAJax = () =>
    Js.Promise.(
      loginData(
        state.userid,
        state.password,
        "longitude" |> Locals.select,
        "latitude" |> Locals.select,
      )
      |> Axiosapi.Login.loginUser
      |> then_(response => {
           (
             switch (response##data##status) {
             | "istrue" =>
               response##data##newid |> Locals.create("newid");
               response##data##name |> Locals.create("name");
               response##data##allname |> Locals.create("allname");
               "autoPath"
               |> Sessions.select
               |> checkObjects
               |> autoLoginPath
               |> ReasonReactRouter.push;
             | _ =>
               restoreAction();
               response##data##status |> accountModule |> barShowRestoreAction;
             }
           )
           |> resolve
         })
      |> catch(error => error |> Js.log |> resolve)
      |> ignore
    );

  let keydownUserid =
    useCallback(keyCode =>
      if (keyCode == 13) {
        ActionOtherLoad(true) |> dispatch;
        loginUserAJax();
      }
    );

  let keydownPassword =
    useCallback(keyCode =>
      if (keyCode == 13) {
        ActionOtherLoad(true) |> dispatch;
        loginUserAJax();
      }
    );

  let sendForm =
    useCallback(_ => {
      ActionOtherLoad(true) |> dispatch;
      loginUserAJax();
    });

  <YoutubeLogin
    error={state.error}
    loading={state.loading}
    showYoutube={state.showYoutube}
    youtubeText={state.youtubeText}
    disabled={state.disabled}
    userid={state.userid}
    changeUserid={event =>
      ReactEvent.Form.target(event)##value |> changeUserid
    }
    keydownUserid={event =>
      ReactEvent.Keyboard.keyCode(event) |> keydownUserid
    }
    password={state.password}
    changePassword={event =>
      ReactEvent.Form.target(event)##value |> changePassword
    }
    keydownPassword={event =>
      ReactEvent.Keyboard.keyCode(event) |> keydownPassword
    }
    forgetForm
    sendForm>
    <SelectOutline
      top="16"
      right="0"
      bottom="8"
      left="0"
      tile="SYSTEM"
      style={ReactDOMRe.Style.make(~padding="18.5px 38px 18.5px 14px", ())}
      value={state.system}
      disabled={state.disabled}
      onClick=showMenuItem>
      ...(
           state.showMenu
             ? <SelectMenu
                 top="50%"
                 transform="translate(0, -50%)"
                 
                 maxHeight="280"
                 minHeight="0"
                 topLeft="12"
                 topRight="12"
                 bottomRight="12"
                 bottomLeft="12"
                 paddingRight="8"
                 paddingLeft="8">
                 {state.optionitems
                  |> Array.map(optionitem =>
                       <MenuItem
                         top="0"
                         right="8"
                         bottom="0"
                         left="8"
                         disablePadding={optionitem.optionPadding}
                         topLeft="12"
                         topRight="12"
                         bottomRight="12"
                         bottomLeft="12"
                         onClick={_ => optionitem.value |> clickMenuItem}>
                         {optionitem.value |> string}
                       </MenuItem>
                     )
                  |> array}
               </SelectMenu>
             : null,
           <IconGeneral
             animation={state.showMenu |> topDownRorate}
             src=arrowDownBlack
           />,
         )
    </SelectOutline>
    <BackgroundBoard
      showBackground={state.showMenu}
      backgroundColor="transparent"
      onClick=showMenuItem
    />
  </YoutubeLogin>;
};
