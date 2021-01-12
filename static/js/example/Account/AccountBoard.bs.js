// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE

import * as List from "bs-platform/lib/es6/list.js";
import * as $$Array from "bs-platform/lib/es6/array.js";
import * as Curry from "bs-platform/lib/es6/curry.js";
import * as React from "react";
import * as ReactIntl from "react-intl";
import * as Tab$BtsCore from "../../material-ui/core/Tabs/Tab.bs.js";
import * as Path$BtsCore from "../../features/Path.bs.js";
import * as Tabs$BtsCore from "../../material-ui/core/Tabs/Tabs.bs.js";
import * as Icons$BtsCore from "../../material-ui/icon/Icons.bs.js";
import * as Paper$BtsCore from "../../material-ui/core/Paper/Paper.bs.js";
import * as Setting$BtsCore from "../../setting/Setting.bs.js";
import * as GridItem$BtsCore from "../../material-ui/core/Grid/GridItem.bs.js";
import * as ReasonReactRouter from "reason-react/src/ReasonReactRouter.js";
import * as IconAction$BtsCore from "../../material-ui/core/IconStyle/IconAction.bs.js";
import * as IconButton$BtsCore from "../../material-ui/core/IconButton/IconButton.bs.js";
import * as Typography$BtsCore from "../../material-ui/core/Typography/Typography.bs.js";
import * as ObjectFormat$BtsCore from "../../controls/ObjectFormat.bs.js";
import * as GridContainer$BtsCore from "../../material-ui/core/Grid/GridContainer.bs.js";
import * as ProgressLinear$BtsCore from "../../material-ui/core/Progress/ProgressLinear.bs.js";
import * as SnackbarYoutube$BtsCore from "../../material-ui/core/Snackbar/SnackbarYoutube.bs.js";

((require('../../../scss/example/Account/accountBoard.scss')));

function reducer(state, action) {
  if (action) {
    return {
            formLoad: state.formLoad,
            formWidth: action[0],
            formHeight: action[1],
            tabitems: state.tabitems
          };
  } else {
    return {
            formLoad: !state.formLoad,
            formWidth: state.formWidth,
            formHeight: state.formHeight,
            tabitems: state.tabitems
          };
  }
}

var initialState_tabitems = /* :: */[
  {
    showTab: false,
    tabImage: Icons$BtsCore.accountBoxBlack
  },
  /* :: */[
    {
      showTab: false,
      tabImage: Icons$BtsCore.accountTreeBlack
    },
    /* :: */[
      {
        showTab: false,
        tabImage: Icons$BtsCore.homeBlack
      },
      /* [] */0
    ]
  ]
];

var initialState = {
  formLoad: false,
  formWidth: 0,
  formHeight: 0,
  tabitems: initialState_tabitems
};

function errorAnimation(error) {
  if (error) {
    return "justLeftRightAnimation 0.5s";
  } else {
    return "";
  }
}

function AccountBoard(Props) {
  var error = Props.error;
  var loading = Props.loading;
  var index = Props.index;
  var tile = Props.tile;
  var showYoutube = Props.showYoutube;
  var youtubeText = Props.youtubeText;
  var children = Props.children;
  var match = React.useReducer(reducer, initialState);
  var dispatch = match[1];
  var state = match[0];
  React.useEffect((function () {
          if (state.formLoad) {
            return (function (param) {
                      console.log("action");
                      
                    });
          }
          Curry._1(dispatch, /* SettingFormLoad */0);
          var sizeId = Curry._1(dispatch, /* SettingFormWidth */[
                window.innerWidth,
                window.innerHeight
              ]);
          return (function (param) {
                    return sizeId;
                  });
        }));
  var handleResize = function ($$event) {
    return Curry._1(dispatch, /* SettingFormWidth */[
                $$event.currentTarget.innerWidth,
                $$event.currentTarget.innerHeight
              ]);
  };
  React.useEffect((function () {
          window.addEventListener("resize", handleResize, true);
          return (function (param) {
                    
                  });
        }), ([]));
  var clickItemTab = React.useCallback((function (i) {
          if (i === 0) {
            return ReasonReactRouter.push(Path$BtsCore.loginPath);
          } else if (i === 1) {
            return ReasonReactRouter.push(Path$BtsCore.signupPath);
          } else if (i === 2) {
            return ReasonReactRouter.push(Path$BtsCore.oauthPath);
          } else {
            return ;
          }
        }));
  var error$1 = Setting$BtsCore.disabledObjects(error);
  return React.createElement("div", undefined, React.createElement(Paper$BtsCore.make, {
                  style: {
                    left: "50%",
                    minHeight: "500px",
                    position: "absolute",
                    textAlign: "center",
                    top: "50%",
                    animation: error$1 ? "justLeftRightAnimation 0.5s" : "",
                    transform: "translate(-50%, -50%)"
                  },
                  className: "paperAccount",
                  children: null
                }, Setting$BtsCore.disabledObjects(loading) ? React.createElement(ProgressLinear$BtsCore.make, {
                        style: {
                          left: "0",
                          position: "fixed",
                          right: "0",
                          top: "0",
                          borderRadius: "4px"
                        }
                      }) : null, React.createElement(GridContainer$BtsCore.make, {
                      direction: "column",
                      justify: "center",
                      alignItem: "stretch",
                      children: null
                    }, React.createElement(GridItem$BtsCore.make, {
                          top: "0",
                          right: "0",
                          bottom: "0",
                          left: "0",
                          xs: "auto",
                          children: React.createElement(Tabs$BtsCore.make, {
                                display: "block",
                                id: "account-",
                                index: Setting$BtsCore.intObjects(index),
                                height: "3",
                                children: $$Array.of_list(List.mapi((function (i, tabitem) {
                                            return React.createElement(Tab$BtsCore.make, {
                                                        showTab: tabitem.showTab,
                                                        borderRadius: "15",
                                                        id: "account-" + String(i),
                                                        animationName: "none",
                                                        onClick: (function (param) {
                                                            return Curry._1(clickItemTab, i);
                                                          }),
                                                        children: React.createElement(IconAction$BtsCore.make, {
                                                              width: "28",
                                                              height: "28",
                                                              animation: "leftRight",
                                                              src: tabitem.tabImage
                                                            })
                                                      });
                                          }), state.tabitems))
                              })
                        }), React.createElement(GridItem$BtsCore.make, {
                          top: "0",
                          xs: "auto",
                          children: null
                        }, React.createElement(Typography$BtsCore.make, {
                              variant: "tile",
                              style: {
                                fontSize: "24px",
                                fontWeight: "400",
                                lineHeight: "1.3333",
                                paddingTop: "16px"
                              },
                              children: Setting$BtsCore.stringObjects(tile)
                            }), React.createElement(Typography$BtsCore.make, {
                              variant: "tile",
                              style: {
                                fontSize: "16px",
                                letterSpacing: "1px",
                                lineHeight: "1.5",
                                paddingTop: "8px"
                              },
                              children: React.createElement(ReactIntl.FormattedMessage, {
                                    id: "Account.info",
                                    defaultMessage: "Info"
                                  })
                            })), React.createElement(GridItem$BtsCore.make, {
                          top: "6",
                          bottom: "0",
                          left: "0",
                          xs: "auto",
                          children: null
                        }, React.createElement(IconButton$BtsCore.make, {
                              onClick: (function (param) {
                                  return ReasonReactRouter.push(Path$BtsCore.loginPath);
                                }),
                              children: React.createElement(IconAction$BtsCore.make, {
                                    animation: "leftRight",
                                    src: Icons$BtsCore.personBlack
                                  })
                            }), "YOUR ACCOUNT : " + ObjectFormat$BtsCore.checkObjects(sessionStorage.getItem("userid"))), React.createElement(GridItem$BtsCore.make, {
                          top: "0",
                          xs: "auto",
                          children: children
                        }))), React.createElement(SnackbarYoutube$BtsCore.make, {
                  showYoutube: Setting$BtsCore.disabledObjects(showYoutube),
                  position: "bottomLeft",
                  children: /* tuple */[
                    React.createElement("span", undefined, Setting$BtsCore.stringObjects(youtubeText)),
                    null
                  ]
                }));
}

var make = AccountBoard;

export {
  reducer ,
  initialState ,
  errorAnimation ,
  make ,
  
}
/*  Not a pure module */
