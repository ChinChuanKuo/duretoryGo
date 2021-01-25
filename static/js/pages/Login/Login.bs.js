// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE

import * as $$Array from "bs-platform/lib/es6/array.js";
import * as Block from "bs-platform/lib/es6/block.js";
import * as Curry from "bs-platform/lib/es6/curry.js";
import * as React from "react";
import * as Data$BtsCore from "../../features/Data.bs.js";
import * as Path$BtsCore from "../../features/Path.bs.js";
import * as Basic$BtsCore from "../../setting/Basic.bs.js";
import * as Icons$BtsCore from "../../material-ui/icon/Icons.bs.js";
import * as Status$BtsCore from "../../features/Status.bs.js";
import * as Axiosapi$BtsCore from "../../features/Axiosapi.bs.js";
import * as MenuItem$BtsCore from "../../material-ui/core/MenuItem/MenuItem.bs.js";
import * as ReasonReactRouter from "reason-react/src/ReasonReactRouter.js";
import * as SelectMenu$BtsCore from "../../material-ui/core/Menu/SelectMenu.bs.js";
import * as IconGeneral$BtsCore from "../../material-ui/core/IconStyle/IconGeneral.bs.js";
import * as ObjectFormat$BtsCore from "../../controls/ObjectFormat.bs.js";
import * as YoutubeLogin$BtsCore from "../../example/Account/Login/YoutubeLogin.bs.js";
import * as IconAnimation$BtsCore from "../../controls/IconAnimation.bs.js";
import * as SelectOutline$BtsCore from "../../material-ui/core/Select/SelectOutline.bs.js";
import * as BackgroundBoard$BtsCore from "../../example/Boards/BackgroundBoard.bs.js";

function reducer(state, action) {
  if (typeof action === "number") {
    return {
            formLoad: state.formLoad,
            error: state.error,
            loading: state.loading,
            showYoutube: state.showYoutube,
            youtubeText: state.youtubeText,
            disabled: state.disabled,
            showMenu: !state.showMenu,
            system: state.system,
            optionitems: state.optionitems,
            userid: state.userid,
            password: state.password
          };
  }
  switch (action.tag | 0) {
    case /* SettingUserId */0 :
        return {
                formLoad: !state.formLoad,
                error: state.error,
                loading: state.loading,
                showYoutube: state.showYoutube,
                youtubeText: state.youtubeText,
                disabled: state.disabled,
                showMenu: state.showMenu,
                system: state.system,
                optionitems: state.optionitems,
                userid: action[0],
                password: state.password
              };
    case /* SettingFormItems */1 :
        return {
                formLoad: state.formLoad,
                error: state.error,
                loading: state.loading,
                showYoutube: state.showYoutube,
                youtubeText: state.youtubeText,
                disabled: state.disabled,
                showMenu: state.showMenu,
                system: state.system,
                optionitems: action[0],
                userid: state.userid,
                password: state.password
              };
    case /* ClickMenuItem */2 :
        return {
                formLoad: state.formLoad,
                error: state.error,
                loading: state.loading,
                showYoutube: state.showYoutube,
                youtubeText: state.youtubeText,
                disabled: state.disabled,
                showMenu: !state.showMenu,
                system: action[0],
                optionitems: state.optionitems,
                userid: state.userid,
                password: state.password
              };
    case /* ChangeUserId */3 :
        return {
                formLoad: state.formLoad,
                error: state.error,
                loading: state.loading,
                showYoutube: state.showYoutube,
                youtubeText: state.youtubeText,
                disabled: state.disabled,
                showMenu: state.showMenu,
                system: state.system,
                optionitems: state.optionitems,
                userid: action[0],
                password: state.password
              };
    case /* ChangePassword */4 :
        return {
                formLoad: state.formLoad,
                error: state.error,
                loading: state.loading,
                showYoutube: state.showYoutube,
                youtubeText: state.youtubeText,
                disabled: state.disabled,
                showMenu: state.showMenu,
                system: state.system,
                optionitems: state.optionitems,
                userid: state.userid,
                password: action[0]
              };
    case /* ActionOtherLoad */5 :
        var other = action[0];
        return {
                formLoad: state.formLoad,
                error: state.error,
                loading: other,
                showYoutube: state.showYoutube,
                youtubeText: state.youtubeText,
                disabled: other,
                showMenu: state.showMenu,
                system: state.system,
                optionitems: state.optionitems,
                userid: state.userid,
                password: state.password
              };
    case /* ActionErrorLoad */6 :
        return {
                formLoad: state.formLoad,
                error: action[0],
                loading: state.loading,
                showYoutube: state.showYoutube,
                youtubeText: state.youtubeText,
                disabled: state.disabled,
                showMenu: state.showMenu,
                system: state.system,
                optionitems: state.optionitems,
                userid: state.userid,
                password: state.password
              };
    case /* ActionSnackBar */7 :
        return {
                formLoad: state.formLoad,
                error: state.error,
                loading: state.loading,
                showYoutube: action[1],
                youtubeText: action[0],
                disabled: state.disabled,
                showMenu: state.showMenu,
                system: state.system,
                optionitems: state.optionitems,
                userid: state.userid,
                password: state.password
              };
    
  }
}

var initialState_optionitems = [];

var initialState = {
  formLoad: false,
  error: false,
  loading: false,
  showYoutube: false,
  youtubeText: "",
  disabled: false,
  showMenu: false,
  system: "",
  optionitems: initialState_optionitems,
  userid: "",
  password: ""
};

function autoLoginPath(value) {
  if (value === "") {
    return "/";
  } else {
    return value;
  }
}

function Login(Props) {
  var match = React.useReducer(reducer, initialState);
  var dispatch = match[1];
  var state = match[0];
  var searchAJax = function (param) {
    Axiosapi$BtsCore.Login.search(Data$BtsCore.userData(localStorage.getItem("newid"))).then((function (response) {
              return Promise.resolve(Curry._1(dispatch, /* SettingFormItems */Block.__(1, [response.data.items])));
            })).catch((function (error) {
            return Promise.resolve((console.log(error), undefined));
          }));
    
  };
  React.useEffect((function () {
          if (state.formLoad) {
            return (function (param) {
                      console.log("action");
                      
                    });
          }
          Curry._1(dispatch, /* SettingUserId */Block.__(0, [ObjectFormat$BtsCore.checkObjects(sessionStorage.getItem("userid"))]));
          navigator.geolocation.getCurrentPosition(Basic$BtsCore.$$Location.success, Basic$BtsCore.$$Location.error, Basic$BtsCore.$$Location.items);
          var searchId = searchAJax(undefined);
          return (function (param) {
                    return searchId;
                  });
        }));
  var showMenuItem = React.useCallback((function (param) {
          return Curry._1(dispatch, /* ShowMenuItem */0);
        }));
  var clickMenuItem = React.useCallback((function (value) {
          return Curry._1(dispatch, /* ClickMenuItem */Block.__(2, [value]));
        }));
  var changeUserid = React.useCallback((function (value) {
          Curry._1(dispatch, /* ChangeUserId */Block.__(3, [value]));
          sessionStorage.setItem("userid", value);
          
        }));
  var changePassword = React.useCallback((function (value) {
          return Curry._1(dispatch, /* ChangePassword */Block.__(4, [value]));
        }));
  var restoreAction = function (param) {
    Curry._1(dispatch, /* ActionErrorLoad */Block.__(6, [true]));
    setTimeout((function (param) {
            Curry._1(dispatch, /* ActionErrorLoad */Block.__(6, [false]));
            return Curry._1(dispatch, /* ActionOtherLoad */Block.__(5, [false]));
          }), 500);
    
  };
  var barShowRestoreAction = function (youtubeText) {
    Curry._1(dispatch, /* ActionSnackBar */Block.__(7, [
            youtubeText,
            true
          ]));
    setTimeout((function (param) {
            return Curry._1(dispatch, /* ActionSnackBar */Block.__(7, [
                          "",
                          false
                        ]));
          }), 5000);
    
  };
  var forgetForm = React.useCallback((function (param) {
          Curry._1(dispatch, /* ActionOtherLoad */Block.__(5, [true]));
          Axiosapi$BtsCore.Login.checkUser(Data$BtsCore.userData(state.userid)).then((function (response) {
                    var match = response.data.status;
                    var tmp;
                    if (match === "istrue") {
                      sessionStorage.setItem("newid", response.data.newid);
                      tmp = ReasonReactRouter.push(Path$BtsCore.forgetPath);
                    } else {
                      restoreAction(undefined);
                      tmp = barShowRestoreAction(Status$BtsCore.accountModule(response.data.status));
                    }
                    return Promise.resolve(tmp);
                  })).catch((function (error) {
                  return Promise.resolve((console.log(error), undefined));
                }));
          
        }));
  var loginUserAJax = function (param) {
    Axiosapi$BtsCore.Login.loginUser(Data$BtsCore.loginData(state.userid, state.password, localStorage.getItem("longitude"), localStorage.getItem("latitude"))).then((function (response) {
              var match = response.data.status;
              var tmp;
              if (match === "istrue") {
                localStorage.setItem("newid", response.data.newid);
                localStorage.setItem("name", response.data.name);
                localStorage.setItem("allname", response.data.allname);
                tmp = ReasonReactRouter.push(autoLoginPath(ObjectFormat$BtsCore.checkObjects(sessionStorage.getItem("autoPath"))));
              } else {
                restoreAction(undefined);
                tmp = barShowRestoreAction(Status$BtsCore.accountModule(response.data.status));
              }
              return Promise.resolve(tmp);
            })).catch((function (error) {
            return Promise.resolve((console.log(error), undefined));
          }));
    
  };
  var keydownUserid = React.useCallback((function (keyCode) {
          if (keyCode === 13) {
            Curry._1(dispatch, /* ActionOtherLoad */Block.__(5, [true]));
            return loginUserAJax(undefined);
          }
          
        }));
  var keydownPassword = React.useCallback((function (keyCode) {
          if (keyCode === 13) {
            Curry._1(dispatch, /* ActionOtherLoad */Block.__(5, [true]));
            return loginUserAJax(undefined);
          }
          
        }));
  var sendForm = React.useCallback((function (param) {
          Curry._1(dispatch, /* ActionOtherLoad */Block.__(5, [true]));
          return loginUserAJax(undefined);
        }));
  return React.createElement(YoutubeLogin$BtsCore.make, {
              error: state.error,
              loading: state.loading,
              showYoutube: state.showYoutube,
              youtubeText: state.youtubeText,
              disabled: state.disabled,
              userid: state.userid,
              changeUserid: (function ($$event) {
                  return Curry._1(changeUserid, $$event.target.value);
                }),
              keydownUserid: (function ($$event) {
                  return Curry._1(keydownUserid, $$event.keyCode);
                }),
              password: state.password,
              changePassword: (function ($$event) {
                  return Curry._1(changePassword, $$event.target.value);
                }),
              keydownPassword: (function ($$event) {
                  return Curry._1(keydownPassword, $$event.keyCode);
                }),
              forgetForm: forgetForm,
              sendForm: sendForm,
              children: null
            }, React.createElement(SelectOutline$BtsCore.make, {
                  top: "16",
                  right: "0",
                  bottom: "8",
                  left: "0",
                  tile: "SYSTEM",
                  style: {
                    padding: "18.5px 38px 18.5px 14px"
                  },
                  value: state.system,
                  disabled: state.disabled,
                  onClick: showMenuItem,
                  children: /* tuple */[
                    state.showMenu ? React.createElement(SelectMenu$BtsCore.make, {
                            top: "50%",
                            transform: "translate(0, -50%)",
                            width: "max-content",
                            maxHeight: "280",
                            minHeight: "0",
                            topLeft: "12",
                            topRight: "12",
                            bottomRight: "12",
                            bottomLeft: "12",
                            paddingRight: "8",
                            paddingLeft: "8",
                            children: $$Array.map((function (optionitem) {
                                    return React.createElement(MenuItem$BtsCore.make, {
                                                top: "0",
                                                right: "8",
                                                bottom: "0",
                                                left: "8",
                                                disablePadding: optionitem.optionPadding,
                                                topLeft: "12",
                                                topRight: "12",
                                                bottomRight: "12",
                                                bottomLeft: "12",
                                                onClick: (function (param) {
                                                    return Curry._1(clickMenuItem, optionitem.value);
                                                  }),
                                                children: optionitem.value
                                              });
                                  }), state.optionitems)
                          }) : null,
                    React.createElement(IconGeneral$BtsCore.make, {
                          animation: IconAnimation$BtsCore.topDownRorate(state.showMenu),
                          src: Icons$BtsCore.arrowDownBlack
                        })
                  ]
                }), React.createElement(BackgroundBoard$BtsCore.make, {
                  showBackground: state.showMenu,
                  backgroundColor: "transparent",
                  onClick: showMenuItem
                }));
}

var make = Login;

export {
  reducer ,
  initialState ,
  autoLoginPath ,
  make ,
  
}
/* react Not a pure module */
