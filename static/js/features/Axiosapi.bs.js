// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE

import * as Axios from "bs-axios/src/axios.js";
import * as Axios$1 from "axios";

function checkNew(data) {
  return Axios$1.post("http://10.10.50.50:1250/Form/checkNewData", data);
}

function loginForm(data) {
  return Axios$1.post("http://10.10.50.50:1250/Form/loginFormData", data);
}

function badgeForm(data) {
  return Axios$1.post("http://10.10.50.50:1250/Form/badgeFormData", data, {
              timeout: 30000
            });
}

function permiss(data) {
  return Axios$1.post("http://10.10.50.50:1250/Form/permissData", data);
}

function record(data) {
  return Axios$1.post("http://10.10.50.50:1250/Form/recordData", data);
}

function addCord(data) {
  return Axios$1.post("http://10.10.50.50:1250/Form/addCordData", data);
}

function badge(data) {
  return Axios$1.post("http://10.10.50.50:1250/Form/badgeData", data);
}

var Form = {
  checkNew: checkNew,
  loginForm: loginForm,
  badgeForm: badgeForm,
  permiss: permiss,
  record: record,
  addCord: addCord,
  badge: badge
};

function search(data) {
  return Axios$1.post("http://10.10.50.50:1250/Login/searchData", data);
}

function checkUser(data) {
  return Axios$1.post("http://10.10.50.50:1250/Login/checkUserData", data);
}

function loginUser(data) {
  return Axios$1.post("http://10.10.50.50:1250/Login/loginUserData", data);
}

var Login = {
  search: search,
  checkUser: checkUser,
  loginUser: loginUser
};

function forgetUser(data) {
  return Axios$1.post("http://10.10.50.50:1250/Forget/forgetUserData", data);
}

var Forget = {
  forgetUser: forgetUser
};

function codeUser(data) {
  return Axios$1.post("http://10.10.50.50:1250/Code/codeUserData", data);
}

var Code = {
  codeUser: codeUser
};

function resendUser(data) {
  return Axios$1.post("http://10.10.50.50:1250/Resend/resendUserData", data);
}

var Resend = {
  resendUser: resendUser
};

function signupUser(data) {
  return Axios$1.post("http://10.10.50.50:1250/Signup/signupUserData", data);
}

var Signup = {
  signupUser: signupUser
};

function search$1(data) {
  return Axios$1.post("http://10.10.50.50:1250/Icon/searchData", data);
}

function insert(data) {
  return Axios$1.post("http://10.10.50.50:1250/Icon/insertData", data);
}

var Icon = {
  search: search$1,
  insert: insert
};

function add(data) {
  return Axios$1.post("/Option/addData", data);
}

var $$Option = {
  add: add
};

function upload(formData) {
  return Axios$1.post("http://10.10.50.50:2250/Files/uploadData", formData, {
              headers: Axios.$$Headers.fromObj({
                    "Content-Type": "multipart/form-data"
                  })
            });
}

function website(data) {
  return Axios$1.post("http://10.10.50.50:1250/Files/websiteData", data);
}

function download(data) {
  return Axios$1.post("http://10.10.50.50:1250/Files/downloadData", data);
}

function transfer(data) {
  return Axios$1.post("http://10.10.50.50:1250/Files/transferData", data);
}

function review(data) {
  return Axios$1.post("http://10.10.50.50:1250/Files/reviewData", data);
}

var Files = {
  upload: upload,
  website: website,
  download: download,
  transfer: transfer,
  review: review
};

function search$2(data) {
  return Axios$1.post("http://10.10.50.50:2250/Home/searchData", data);
}

function filter(data) {
  return Axios$1.post("http://10.10.50.50:2250/Home/filterData", data);
}

function scroll(data) {
  return Axios$1.post("http://10.10.50.50:2250/Home/scrollData", data);
}

function sFilter(data) {
  return Axios$1.post("http://10.10.50.50:2250/Home/sFilterData", data);
}

function $$delete(data) {
  return Axios$1.post("http://10.10.50.50:2250/Home/deleteData", data);
}

function sView(data) {
  return Axios$1.post("http://10.10.50.50:2250/Home/sViewData", data);
}

function sItem(data) {
  return Axios$1.post("http://10.10.50.50:2250/Home/sItemData", data);
}

function insert$1(data) {
  return Axios$1.post("http://10.10.50.50:2250/Home/insertData", data);
}

function sRefresh(data) {
  return Axios$1.post("http://10.10.50.50:2250/Home/sRefreshData", data);
}

var Default = {
  search: search$2,
  filter: filter,
  scroll: scroll,
  sFilter: sFilter,
  $$delete: $$delete,
  sView: sView,
  sItem: sItem,
  insert: insert$1,
  sRefresh: sRefresh
};

function search$3(data) {
  return Axios$1.post("http://10.10.50.50:2250/Search/searchData", data);
}

function filter$1(data) {
  return Axios$1.post("http://10.10.50.50:2250/Search/filterData", data);
}

var Search = {
  search: search$3,
  filter: filter$1
};

function search$4(data) {
  return Axios$1.post("http://10.10.50.50:2250/Formor/searchData", data);
}

function insert$2(data) {
  return Axios$1.post("http://10.10.50.50:2250/Formor/insertData", data);
}

var Formor = {
  search: search$4,
  insert: insert$2
};

function search$5(data) {
  return Axios$1.post("http://10.10.50.50:2250/Create/searchData", data);
}

function insert$3(data) {
  return Axios$1.post("http://10.10.50.50:2250/Create/insertData", data);
}

var Create = {
  search: search$5,
  insert: insert$3
};

export {
  Form ,
  Login ,
  Forget ,
  Code ,
  Resend ,
  Signup ,
  Icon ,
  $$Option ,
  Files ,
  Default ,
  Search ,
  Formor ,
  Create ,
  
}
/* axios Not a pure module */
