// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE


function userData(userid) {
  return {
          userid: userid
        };
}

function loginData(userid, password, longitude, latitude) {
  return {
          userid: userid,
          password: password,
          longitude: longitude,
          latitude: latitude
        };
}

function otherData(userid, values) {
  return {
          userid: userid,
          values: values
        };
}

function signupData(userid, password, username, birthday) {
  return {
          userid: userid,
          password: password,
          username: username,
          birthday: birthday
        };
}

function iIconData(items, qaitems, newid) {
  return {
          items: items,
          qaitems: qaitems,
          newid: newid
        };
}

function iSecuData(externip, newid) {
  return {
          externip: externip,
          newid: newid
        };
}

function iNotiData(isbool, newid) {
  return {
          isbool: isbool,
          newid: newid
        };
}

function sRowsData(formId, value, newid) {
  return {
          formId: formId,
          value: value,
          newid: newid
        };
}

function dFormData(formId, newid) {
  return {
          formId: formId,
          newid: newid
        };
}

function iItemsData(items, newid) {
  return {
          items: items,
          newid: newid
        };
}

function iFileData(formId, original, encryption, extension, newid) {
  return {
          formId: formId,
          original: original,
          encryption: encryption,
          extension: extension,
          newid: newid
        };
}

function iFormData(formId, tile, desc, items, newid) {
  return {
          formId: formId,
          tile: tile,
          desc: desc,
          items: items,
          newid: newid
        };
}

function sLocalData(longitude, latitude, value, newid) {
  return {
          longitude: longitude,
          latitude: latitude,
          value: value,
          newid: newid
        };
}

function sScollData(items, value, newid) {
  return {
          items: items,
          value: value,
          newid: newid
        };
}

function sFiltData(items, index, outValue, value, newid) {
  return {
          items: items,
          index: index,
          outValue: outValue,
          value: value,
          newid: newid
        };
}

export {
  userData ,
  loginData ,
  otherData ,
  signupData ,
  iIconData ,
  iSecuData ,
  iNotiData ,
  sRowsData ,
  dFormData ,
  iItemsData ,
  iFileData ,
  iFormData ,
  sLocalData ,
  sScollData ,
  sFiltData ,
  
}
/* No side effect */
