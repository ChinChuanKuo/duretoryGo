// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE

import * as React from "react";
import * as Setting$BtsCore from "../../../setting/Setting.bs.js";

((require('../../scss/Menu/menu.scss')));

function locations($$location) {
  if ($$location !== undefined) {
    return $$location;
  } else {
    return "auto";
  }
}

function otherSizes(otherSize) {
  if (otherSize !== undefined) {
    return otherSize + "px";
  } else {
    return "auto";
  }
}

function widths(width) {
  if (width !== undefined) {
    return width;
  } else {
    return "100%";
  }
}

function transforms(transform) {
  if (transform !== undefined) {
    return transform;
  } else {
    return "translate(0, 0)";
  }
}

function overflows(overflow) {
  if (overflow !== undefined) {
    return overflow;
  } else {
    return "auto";
  }
}

function backgroundColors(backgroundColor) {
  if (backgroundColor !== undefined) {
    return backgroundColor;
  } else {
    return "rgba(255,255,255,1)";
  }
}

function borderRadiuses(borderRadius) {
  if (borderRadius !== undefined) {
    return borderRadius + "px";
  } else {
    return "4px";
  }
}

function paddingTopBottoms(topBottom) {
  if (topBottom !== undefined) {
    return topBottom + "px";
  } else {
    return "8px";
  }
}

function paddingOthers(other) {
  if (other !== undefined) {
    return other + "px";
  } else {
    return "0px";
  }
}

function SelectMenu(Props) {
  var style = Props.style;
  var top = Props.top;
  var right = Props.right;
  var bottom = Props.bottom;
  var left = Props.left;
  var transform = Props.transform;
  var width = Props.width;
  var minWidth = Props.minWidth;
  var maxHeight = Props.maxHeight;
  var height = Props.height;
  var minHeight = Props.minHeight;
  var overflow = Props.overflow;
  var backgroundColor = Props.backgroundColor;
  var topLeft = Props.topLeft;
  var topRight = Props.topRight;
  var bottomRight = Props.bottomRight;
  var bottomLeft = Props.bottomLeft;
  var paddingTop = Props.paddingTop;
  var paddingRight = Props.paddingRight;
  var paddingBottom = Props.paddingBottom;
  var paddingLeft = Props.paddingLeft;
  var children = Props.children;
  return React.createElement("div", {
              className: "jt9nf5x",
              style: Object.assign(({}), {
                    backgroundColor: backgroundColor !== undefined ? backgroundColor : "rgba(255,255,255,1)",
                    bottom: bottom !== undefined ? bottom : "auto",
                    height: otherSizes(height),
                    left: left !== undefined ? left : "auto",
                    maxHeight: otherSizes(maxHeight),
                    minHeight: otherSizes(minHeight),
                    minWidth: minWidth !== undefined ? minWidth : "100%",
                    overflow: overflow !== undefined ? overflow : "auto",
                    position: "absolute",
                    right: right !== undefined ? right : "auto",
                    top: top !== undefined ? top : "auto",
                    width: width !== undefined ? width : "100%",
                    zIndex: "1200",
                    borderTopLeftRadius: borderRadiuses(topLeft),
                    borderTopRightRadius: borderRadiuses(topRight),
                    borderBottomLeftRadius: borderRadiuses(bottomLeft),
                    borderBottomRightRadius: borderRadiuses(bottomRight),
                    transform: transform !== undefined ? transform : "translate(0, 0)"
                  }, Setting$BtsCore.styleObjects(style))
            }, React.createElement("ul", {
                  className: "j1afduyh",
                  style: {
                    paddingTop: paddingTopBottoms(paddingTop),
                    paddingRight: paddingOthers(paddingRight),
                    paddingBottom: paddingTopBottoms(paddingBottom),
                    paddingLeft: paddingOthers(paddingLeft)
                  }
                }, children));
}

var make = SelectMenu;

export {
  locations ,
  otherSizes ,
  widths ,
  transforms ,
  overflows ,
  backgroundColors ,
  borderRadiuses ,
  paddingTopBottoms ,
  paddingOthers ,
  make ,
  
}
/*  Not a pure module */
