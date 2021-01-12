// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE

import * as Block from "bs-platform/lib/es6/block.js";
import * as Curry from "bs-platform/lib/es6/curry.js";
import * as React from "react";
import * as Setting$BtsCore from "../../../setting/Setting.bs.js";

function reducer(state, action) {
  if (action.tag) {
    return {
            enter: state.enter,
            down: action[0]
          };
  }
  var enter = action[0];
  return {
          enter: enter,
          down: enter ? state.down : false
        };
}

var initialState = {
  enter: false,
  down: false
};

function listStyles(listStyle) {
  if (listStyle !== undefined) {
    return listStyle;
  } else {
    return "none";
  }
}

function colors(color) {
  if (color !== undefined) {
    return color;
  } else {
    return "inherit";
  }
}

function MuiBreadcrumb(Props) {
  var style = Props.style;
  var listStyle = Props.listStyle;
  var enterColor = Props.enterColor;
  var downColor = Props.downColor;
  var color = Props.color;
  var disabled = Props.disabled;
  var href = Props.href;
  var children = Props.children;
  var match = React.useReducer(reducer, initialState);
  var dispatch = match[1];
  var state = match[0];
  var match$1 = state.enter;
  var match$2 = state.down;
  var match$3 = state.enter;
  return React.createElement("li", {
              style: Object.assign(({}), {
                    listStyle: listStyle !== undefined ? listStyle : "none"
                  }, Setting$BtsCore.styleObjects(style))
            }, React.createElement("a", {
                  role: "button",
                  style: {
                    color: match$1 ? (
                        match$2 ? (
                            downColor !== undefined ? downColor : "inherit"
                          ) : (
                            enterColor !== undefined ? enterColor : "inherit"
                          )
                      ) : (
                        color !== undefined ? color : "inherit"
                      ),
                    cursor: "pointer",
                    display: "flex",
                    margin: "0",
                    textDecoration: match$3 ? "underline" : "none"
                  },
                  disabled: Setting$BtsCore.disabledObjects(disabled),
                  href: Setting$BtsCore.stringObjects(href),
                  onMouseDown: (function (param) {
                      return Curry._1(dispatch, /* MouseUpDown */Block.__(1, [true]));
                    }),
                  onMouseEnter: (function (param) {
                      return Curry._1(dispatch, /* MouseEnterLeave */Block.__(0, [true]));
                    }),
                  onMouseLeave: (function (param) {
                      return Curry._1(dispatch, /* MouseEnterLeave */Block.__(0, [false]));
                    }),
                  onMouseUp: (function (param) {
                      return Curry._1(dispatch, /* MouseUpDown */Block.__(1, [false]));
                    })
                }, children));
}

var make = MuiBreadcrumb;

export {
  reducer ,
  initialState ,
  listStyles ,
  colors ,
  make ,
  
}
/* react Not a pure module */
