// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE

import * as React from "react";
import * as Setting$BtsCore from "../../setting/Setting.bs.js";

((require('../../../scss/example/Medias/mediaImage.scss')));

function widths(width) {
  if (width !== undefined) {
    return width;
  } else {
    return "auto";
  }
}

function heights(height) {
  if (height !== undefined) {
    return height;
  } else {
    return "200px";
  }
}

function borderRadiuses(borderRadius) {
  if (borderRadius !== undefined) {
    if (borderRadius === "circle") {
      return "50%";
    } else {
      return borderRadius + "px";
    }
  } else {
    return "0px";
  }
}

function MediaImage(Props) {
  var style = Props.style;
  var width = Props.width;
  var height = Props.height;
  var borderRadius = Props.borderRadius;
  var src = Props.src;
  return React.createElement("div", {
              style: Object.assign(({}), {
                    opacity: "0",
                    animation: "fadeIn 0.8s ease-in forwards"
                  }, Setting$BtsCore.styleObjects(style))
            }, React.createElement("img", {
                  style: {
                    cursor: "pointer",
                    height: height !== undefined ? height : "200px",
                    width: width !== undefined ? width : "auto",
                    borderRadius: borderRadiuses(borderRadius)
                  },
                  src: Setting$BtsCore.stringObjects(src)
                }));
}

var make = MediaImage;

export {
  widths ,
  heights ,
  borderRadiuses ,
  make ,
  
}
/*  Not a pure module */