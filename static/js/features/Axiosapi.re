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
  let badge = data =>
    postData("http://10.10.50.50:1250/Form/badgeData", data);
};

module Login = {
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
      "http://localhost:5000/Files/uploadData",
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
    postData("http://localhost:5000/Home/searchData", data);
  let filter = data =>
    postData("http://localhost:5000/Home/filterData", data);
  let scroll = data =>
    postData("http://localhost:5000/Home/scrollData", data);
  let sFilter = data =>
    postData("http://localhost:5000/Home/sFilterData", data);
  let delete = data =>
    postData("http://localhost:5000/Home/deleteData", data);
  let sItem = data => postData("http://localhost:5000/Home/sItemData", data);
  let insert = data =>
    postData("http://localhost:5000/Home/insertData", data);
};

module Formor = {
  let search = data =>
    postData("http://localhost:5000/Formor/searchData", data);
  let insert = data =>
    postData("http://localhost:5000/Formor/insertData", data);
};

module Create = {
  let search = data =>
    postData("http://localhost:5000/Create/searchData", data);
  let insert = data =>
    postData("http://localhost:5000/Create/insertData", data);
};
