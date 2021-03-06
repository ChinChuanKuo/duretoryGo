open Axios;
module Form = {
  let checkNew = data =>
    postData("http://10.10.50.50:1250/Form/checkNewData", data);
  let loginForm = data =>
    postData("http://10.10.50.50:1250/Form/loginFormData", data);
  let badgeForm = data =>
    postDatac(
      "http://10.10.50.50:1250/Form/badgeFormData",
      data,
      makeConfig(~timeout=30000, ()),
    );
  let permiss = data =>
    postData("http://10.10.50.50:1250/Form/permissData", data);
  let record = data =>
    postData("http://10.10.50.50:1250/Form/recordData", data);
  let addCord = data =>
    postData("http://10.10.50.50:1250/Form/addCordData", data);
  let badge = data =>
    postData("http://10.10.50.50:1250/Form/badgeData", data);
};

module Login = {
  let search = data =>
    postData("http://10.10.50.50:1250/Login/searchData", data);
  let checkUser = data =>
    postData("http://10.10.50.50:1250/Login/checkUserData", data);
  let loginUser = data =>
    postData("http://10.10.50.50:1250/Login/loginUserData", data);
};

module Forget = {
  let forgetUser = data =>
    postData("http://10.10.50.50:1250/Forget/forgetUserData", data);
};

module Code = {
  let codeUser = data =>
    postData("http://10.10.50.50:1250/Code/codeUserData", data);
};

module Resend = {
  let resendUser = data =>
    postData("http://10.10.50.50:1250/Resend/resendUserData", data);
};

module Signup = {
  let signupUser = data =>
    postData("http://10.10.50.50:1250/Signup/signupUserData", data);
};

module Icon = {
  let search = data =>
    postData("http://10.10.50.50:1250/Icon/searchData", data);
  let insert = data =>
    postData("http://10.10.50.50:1250/Icon/insertData", data);
};

module Option = {
  let add = data => postData("/Option/addData", data);
};

module Files = {
  let upload = formData =>
    postDatac(
      "http://10.10.50.50:2250/Files/uploadData",
      formData,
      makeConfig(
        ~headers=Headers.fromObj({"Content-Type": "multipart/form-data"}),
        (),
      ),
    );
  let website = data =>
    postData("http://10.10.50.50:1250/Files/websiteData", data);
  let download = data =>
    postData("http://10.10.50.50:1250/Files/downloadData", data);
  let transfer = data =>
    postData("http://10.10.50.50:1250/Files/transferData", data);
  let review = data =>
    postData("http://10.10.50.50:1250/Files/reviewData", data);
};

module Default = {
  /*let search = data =>
    postDatac("/Home/searchData", data, makeConfig(~timeout=30000, ()));*/
  let search = data =>
    postData("http://10.10.50.50:2250/Home/searchData", data);
  let filter = data =>
    postData("http://10.10.50.50:2250/Home/filterData", data);
  let scroll = data =>
    postData("http://10.10.50.50:2250/Home/scrollData", data);
  let sFilter = data =>
    postData("http://10.10.50.50:2250/Home/sFilterData", data);
  let delete = data =>
    postData("http://10.10.50.50:2250/Home/deleteData", data);
  let sView = data =>
    postData("http://10.10.50.50:2250/Home/sViewData", data);
  let sItem = data =>
    postData("http://10.10.50.50:2250/Home/sItemData", data);
  let insert = data =>
    postData("http://10.10.50.50:2250/Home/insertData", data);
  let sRefresh = data =>
    postData("http://10.10.50.50:2250/Home/sRefreshData", data);
};

module Search = {
  let search = data =>
    postData("http://10.10.50.50:2250/Search/searchData", data);
  let filter = data =>
    postData("http://10.10.50.50:2250/Search/filterData", data);
};

module Formor = {
  let search = data =>
    postData("http://10.10.50.50:2250/Formor/searchData", data);
  let insert = data =>
    postData("http://10.10.50.50:2250/Formor/insertData", data);
};

module Create = {
  let search = data =>
    postData("http://10.10.50.50:2250/Create/searchData", data);
  let insert = data =>
    postData("http://10.10.50.50:2250/Create/insertData", data);
};
