// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE

import * as React from "react";
import * as Setting$BtsCore from "../../../setting/Setting.bs.js";

((require('../../scss/Card/card.scss')));

function sizes(size) {
  if (size !== undefined) {
    return size;
  } else {
    return "100%";
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

function Card(Props) {
  var style = Props.style;
  var width = Props.width;
  var height = Props.height;
  var backgroundColor = Props.backgroundColor;
  var topLeft = Props.topLeft;
  var topRight = Props.topRight;
  var bottomRight = Props.bottomRight;
  var bottomLeft = Props.bottomLeft;
  var children = Props.children;
  return React.createElement("div", {
              className: "j1nljlifc j6afj2p jlixhhq",
              style: Object.assign(({}), {
                    backgroundColor: backgroundColor !== undefined ? backgroundColor : "rgba(255,255,255,1)",
                    height: height !== undefined ? height : "100%",
                    maxWidth: width !== undefined ? width : "100%",
                    minWidth: width !== undefined ? width : "100%",
                    width: width !== undefined ? width : "100%",
                    borderTopLeftRadius: borderRadiuses(topLeft),
                    borderTopRightRadius: borderRadiuses(topRight),
                    borderBottomLeftRadius: borderRadiuses(bottomLeft),
                    borderBottomRightRadius: borderRadiuses(bottomRight)
                  }, Setting$BtsCore.styleObjects(style))
            }, children);
}

var make = Card;

export {
  sizes ,
  backgroundColors ,
  borderRadiuses ,
  make ,
  
}
/*  Not a pure module */
