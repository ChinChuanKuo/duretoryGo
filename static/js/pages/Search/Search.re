open React;
open Together;
open ReactIntl;
open Icons;
open Data;
open Items;
open Axiosapi;
open Status;
open Storage;
open AnswerColor;
open IconAnimation;
[%bs.raw {|require('../../../scss/pages/Together/together.scss')|}];

type answeritem = {
  id: int,
  value: string,
  showAnswer: bool,
};

type collitem = {
  id: int,
  showImage: bool,
  showVideo: bool,
  showAudio: bool,
  value: string,
  showDelete: bool,
  collDelete: bool,
};

type formitem = {
  iid: int,
  title: string,
  values: string,
  showMenu: bool,
  showDrop: bool,
  showFile: bool,
  showImage: bool,
  showVideo: bool,
  showAudio: bool,
  outValue: string,
  showShow: bool,
  showCheck: bool,
  showFilter: bool,
  collIndex: int,
  collitems: array(collitem),
  optionitems: array(optionitem),
  answeritems: array(answeritem),
  formModify: bool,
};

type dataitem = {
  key: string,
  data: string,
};

type viewitem = {
  viewIndex: int,
  viewections: array(string),
  dataitems: array(dataitem),
};

type filtitem = {
  filtIndex: int,
  filtTile: string,
  filtOutValue: string,
  filtValue: string,
  filtMenu: bool,
  filtOptions: array(optionitem),
};

type item = {
  id: string,
  index: int,
  collections: array(string),
  tile: string,
  creator: string,
  datetime: string,
  itemDelete: bool,
};

type state = {
  formLoad: bool,
  formWidth: int,
  formHeight: int,
  showProgress: bool,
  error: bool,
  insert: bool,
  update: bool,
  delete: bool,
  export: bool,
  itemId: string,
  filtitems: array(filtitem),
  showItem: bool,
  itemCount: int,
  items: array(item),
  viewFull: bool,
  viewId: string,
  viewTitle: string,
  viewitems: array(viewitem),
  showFull: bool,
  formIndex: int,
  formId: string,
  formTitle: string,
  formitems: array(formitem),
  showYoutube: bool,
  youtubeText: string,
};

let newcollitem = (id, showImage, showVideo, showAudio, value) => [|
  {
    id,
    showImage,
    showVideo,
    showAudio,
    value,
    showDelete: true,
    collDelete: false,
  },
|];

type action =
  | SettingError
  | SettingFormLoad(string)
  | SettingFormWidth(int, int)
  | ActionShowProgress
  | ActionPermissItems(bool, bool, bool, bool)
  | SettingFiltItems(array(filtitem))
  | SettingFormItems(bool, int, array(item))
  | SettingScrollItems(bool, array(item))
  | SettingSingleItem(int, array(item))
  | ShowAnimationViewFull(string, string, array(viewitem))
  | ShowAnimationFull(int, string, string, array(formitem))
  | ShowFiltMenu(int)
  | ClickFiltMenu(string, int)
  | ShowCollections(int, int)
  | ClearForm(string)
  | SettingViewCollection(int, int)
  | ShowDrop(bool, int)
  | ShowFile(bool, bool, bool, string, int)
  | ShowFiles(bool, bool, bool, string, int)
  | SettingCollection(int, int)
  | ActionCollection(int, int)
  | ChangeItem(string, int)
  | ShowMenuItem(int)
  | ClickMenuItem(string, int)
  | ClickRadioItem(int, int)
  | ClickCheckboxItem(int, int)
  | CloseAnimationViewFull
  | CloseAnimationFull
  | ActionSnackBar(string, bool);

let reducer = (state, action) =>
  switch (action) {
  | SettingError => {...state, error: !state.error}
  | SettingFormLoad(value) => {
      ...state,
      itemId: value,
      formLoad: !state.formLoad,
    }
  | SettingFormWidth(width, height) => {
      ...state,
      formWidth: width,
      formHeight: height,
    }
  | ActionShowProgress => {...state, showProgress: !state.showProgress}
  | ActionPermissItems(insert, update, delete, export) => {
      ...state,
      insert,
      update,
      delete,
      export,
    }
  | SettingFiltItems(filtitems) => {...state, filtitems}
  | SettingFormItems(showItem, itemCount, items) => {
      ...state,
      showItem,
      itemCount,
      items,
    }
  | SettingScrollItems(showItem, items) => {
      ...state,
      showItem,
      items: Array.append(state.items, items),
    }
  | SettingSingleItem(index, items) => {
      ...state,
      items:
        Array.mapi(
          (i, item) =>
            index == i
              ? {
                ...item,
                index: 0,
                collections: items[0].collections,
                tile: items[0].tile,
                datetime: items[0].datetime,
              }
              : item,
          state.items,
        ),
    }
  | ShowAnimationViewFull(id, value, viewitems) => {
      ...state,
      viewId: id,
      viewTitle: value,
      viewitems,
      viewFull: !state.viewFull,
    }
  | ShowAnimationFull(index, id, value, formitems) => {
      ...state,
      formIndex: index,
      formId: id,
      formTitle: value,
      formitems,
      showFull: !state.showFull,
    }
  | ShowFiltMenu(index) => {
      ...state,
      filtitems:
        Array.mapi(
          (i, filtitem) =>
            index == i
              ? {...filtitem, filtMenu: !filtitem.filtMenu} : filtitem,
          state.filtitems,
        ),
    }
  | ClickFiltMenu(value, index) => {
      ...state,
      filtitems:
        Array.mapi(
          (i, filtitem) =>
            index == i
              ? {...filtitem, filtValue: value, filtMenu: !filtitem.filtMenu}
              : filtitem,
          state.filtitems,
        ),
    }
  | ShowCollections(cIndex, index) => {
      ...state,
      items:
        Array.mapi(
          (i, item) => index == i ? {...item, index: cIndex} : item,
          state.items,
        ),
    }
  | ClearForm(id) => {
      ...state,
      itemCount: state.itemCount - 1,
      error: state.itemCount == 1,
      items: Js_array.filter((item: item) => item.id !== id, state.items),
    }
  | SettingViewCollection(viewIndex, index) => {
      ...state,
      viewitems:
        Array.mapi(
          (i, item) => index == i ? {...item, viewIndex} : item,
          state.viewitems,
        ),
    }
  | ShowDrop(droped, index) => {
      ...state,
      formitems:
        Array.mapi(
          (i, item) => index == i ? {...item, showDrop: droped} : item,
          state.formitems,
        ),
    }
  | ShowFile(showImage, showVideo, showAudio, values, index) => {
      ...state,
      formitems:
        Array.mapi(
          (i, item) =>
            index == i
              ? {
                ...item,
                showImage,
                showVideo,
                showAudio,
                values,
                showFile: true,
                formModify: true,
              }
              : item,
          state.formitems,
        ),
    }
  | ShowFiles(showImage, showVideo, showAudio, values, index) => {
      ...state,
      formitems:
        Array.mapi(
          (i, item) =>
            index == i
              ? {
                ...item,
                collitems:
                  Array.append(
                    item.collitems,
                    newcollitem(
                      Js_array.length(item.collitems) + 1,
                      showImage,
                      showVideo,
                      showAudio,
                      values,
                    ),
                  ),
                values,
                showFile: true,
                formModify: true,
              }
              : item,
          state.formitems,
        ),
    }
  | SettingCollection(cIndex, index) => {
      ...state,
      formitems:
        Array.mapi(
          (i, item) => index == i ? {...item, collIndex: cIndex} : item,
          state.formitems,
        ),
    }
  | ActionCollection(cIndex, index) => {
      ...state,
      formitems:
        Array.mapi(
          (i, item) =>
            index == i
              ? {
                ...item,
                collitems:
                  Array.mapi(
                    (ci, collitem) =>
                      cIndex == ci
                        ? {...collitem, collDelete: !collitem.collDelete}
                        : collitem,
                    item.collitems,
                  ),
                formModify: true,
              }
              : item,
          state.formitems,
        ),
    }
  | ChangeItem(value, index) => {
      ...state,
      formitems:
        Array.mapi(
          (i, item) =>
            index == i ? {...item, values: value, formModify: true} : item,
          state.formitems,
        ),
    }
  | ShowMenuItem(index) => {
      ...state,
      formitems:
        Array.mapi(
          (i, item) =>
            index == i ? {...item, showMenu: !item.showMenu} : item,
          state.formitems,
        ),
    }
  | ClickMenuItem(value, index) => {
      ...state,
      formitems:
        Array.mapi(
          (i, item) =>
            index == i
              ? {
                ...item,
                values: value,
                showMenu: !item.showMenu,
                formModify: true,
              }
              : item,
          state.formitems,
        ),
    }
  | ClickRadioItem(rindex, index) => {
      ...state,
      formitems:
        Array.mapi(
          (i, item) =>
            index == i
              ? {
                ...item,
                answeritems:
                  Array.mapi(
                    (ri, answeritem) =>
                      {
                        ...answeritem,
                        showAnswer:
                          rindex == ri ? !answeritem.showAnswer : false,
                      },
                    item.answeritems,
                  ),
                formModify: true,
              }
              : item,
          state.formitems,
        ),
    }
  | ClickCheckboxItem(rindex, index) => {
      ...state,
      formitems:
        Array.mapi(
          (i, item) =>
            index == i
              ? {
                ...item,
                answeritems:
                  Array.mapi(
                    (ri, answeritem) =>
                      rindex == ri
                        ? {...answeritem, showAnswer: !answeritem.showAnswer}
                        : answeritem,
                    item.answeritems,
                  ),
                formModify: true,
              }
              : item,
          state.formitems,
        ),
    }
  | CloseAnimationViewFull => {...state, viewFull: !state.viewFull}
  | CloseAnimationFull => {...state, showFull: !state.showFull}
  | ActionSnackBar(youtubeText, showYoutube) => {
      ...state,
      youtubeText,
      showYoutube,
    }
  };

let initialState = {
  formLoad: false,
  formWidth: 0,
  formHeight: 0,
  showProgress: true,
  error: false,
  insert: false,
  update: false,
  delete: false,
  export: false,
  itemId: "",
  filtitems: [||],
  showItem: false,
  itemCount: 0,
  items: [||],
  viewFull: false,
  viewId: "",
  viewTitle: "",
  viewitems: [||],
  showFull: false,
  formIndex: 0,
  formId: "",
  formTitle: "",
  formitems: [||],
  showYoutube: false,
  youtubeText: "",
};

let positionRelative = ReactDOMRe.Style.make(~position="relative", ());

let outsideCollections =
  ReactDOMRe.Style.make(
    ~position="absolute",
    ~top="100%",
    ~transform="translate(0, -150%)",
    ~zIndex="1",
    (),
  );

let insideCollections =
  ReactDOMRe.Style.make(
    ~position="absolute",
    ~top="50%",
    ~transform="translate(0px, -50%)",
    ~zIndex="1",
    (),
  );

[@react.component]
let make = _ => {
  let (state, dispatch) = useReducer(reducer, initialState);

  let fileRef = useRef(Js.Nullable.null);

  let barShowRestoreAction = youtubeText => {
    ActionSnackBar(youtubeText, true) |> dispatch;
    Js.Global.setTimeout(() => ActionSnackBar("", false) |> dispatch, 5000)
    |> ignore;
  };

  let searchAJax = itemId =>
    Js.Promise.(
      "newid"
      |> Locals.select
      |> sRowsData(itemId, state.items |> Js_array.length |> string_of_int)
      |> Axiosapi.Search.search
      |> then_(response =>
           {
             switch (response##data##status) {
             | "istrue" =>
               SettingFormItems(
                 response##data##showItem,
                 response##data##itemCount,
                 response##data##items,
               )
               |> dispatch;
               ActionShowProgress |> dispatch;
             | _ =>
               SettingError |> dispatch;
               response##data##status |> statusModule |> barShowRestoreAction;
               ActionShowProgress |> dispatch;
             };
           }
           |> resolve
         )
      |> catch(error => error |> Js.log |> resolve)
      |> ignore
    );

  let filterAJax = itemId =>
    Js.Promise.(
      "newid"
      |> Locals.select
      |> dFormData(itemId)
      |> Axiosapi.Search.filter
      |> then_(response =>
           {
             SettingFiltItems(response##data##items) |> dispatch;
             itemId |> searchAJax;
           }
           |> resolve
         )
      |> catch(error => error |> Js.log |> resolve)
      |> ignore
    );

  let permissAJax = () =>
    Js.Promise.(
      "newid"
      |> Locals.select
      |> userData
      |> Form.permiss
      |> then_(response =>
           {
             ActionPermissItems(
               response##data##insert,
               response##data##update,
               response##data##delete,
               response##data##export,
             )
             |> dispatch;
             ReasonReactRouter.dangerouslyGetInitialUrl().hash |> filterAJax;
           }
           |> resolve
         )
      |> catch(error => error |> Js.log |> resolve)
      |> ignore
    );

  useEffect(() =>
    switch (state.formLoad) {
    | true => Some(() => "action" |> Js.log)
    | _ =>
      let testtime =
        SettingFormLoad(ReasonReactRouter.dangerouslyGetInitialUrl().hash)
        |> dispatch;
      let sizeId =
        SettingFormWidth(Window.Sizes.width, Window.Sizes.height) |> dispatch;
      let timeId = permissAJax();
      Some(
        () => {
          testtime;
          sizeId;
          timeId;
        },
      );
    }
  );

  let handleResize = event =>
    SettingFormWidth(
      event##currentTarget##innerWidth,
      event##currentTarget##innerHeight,
    )
    |> dispatch;

  useEffect0(() => {
    let watchId =
      ReasonReactRouter.watchUrl(url => {
        SettingFormLoad(ReasonReactRouter.dangerouslyGetInitialUrl().hash)
        |> dispatch;
        ActionShowProgress |> dispatch;
      });
    let sizeId = Window.Listeners.add("resize", handleResize, true) |> ignore;
    /*let scrollId =
      Window.Listeners.add("scroll", handleScrollBar, true) |> ignore;*/
    Some(
      () => {
        watchId |> ReasonReactRouter.unwatchUrl;
        sizeId;
      },
    );
  });

  let clickScrollBar = useCallback(_ => ActionShowProgress |> dispatch);

  let showFiltMenu = useCallback(index => ShowFiltMenu(index) |> dispatch);

  let sfilterAJax = (index, outValue, value) =>
    Js.Promise.(
      "newid"
      |> Locals.select
      |> sFiltData(
           Js_array.filter(
             (filtitem: filtitem) => filtitem.filtValue !== "",
             state.filtitems,
           ),
           index,
           outValue,
           value,
         )
      |> Default.sFilter
      |> then_(response =>
           {
             SettingFormItems(
               response##data##showItem,
               response##data##itemCount,
               response##data##items,
             )
             |> dispatch;
             response##data##status |> statusModule |> barShowRestoreAction;
           }
           |> resolve
         )
      |> catch(error => error |> Js.log |> resolve)
      |> ignore
    );

  let clickFiltMenu =
    useCallback((value, itemIndex, outValue, index) => {
      ClickFiltMenu(value, index) |> dispatch;
      value |> sfilterAJax(itemIndex, outValue);
    });

  let showPreviousCollections =
    useCallback((id, index, event) => {
      ReactEvent.Mouse.preventDefault(event);
      ReactEvent.Mouse.stopPropagation(event);
      let length = Js_array.length(state.items[index].collections) - 1;
      let collIndex = id == 0 ? length : id - 1;
      ShowCollections(collIndex, index) |> dispatch;
    });

  let showNextCollections =
    useCallback((id, index, event) => {
      ReactEvent.Mouse.preventDefault(event);
      ReactEvent.Mouse.stopPropagation(event);
      let length = Js_array.length(state.items[index].collections) - 1;
      let collIndex = id == length ? 0 : id + 1;
      ShowCollections(collIndex, index) |> dispatch;
    });

  let sViewAJax = id =>
    Js.Promise.(
      "newid"
      |> Locals.select
      |> dFormData(id)
      |> Default.sView
      |> then_(response => {
           (
             switch (response##data##status) {
             | "istrue" =>
               ShowAnimationViewFull(
                 id,
                 response##data##tile,
                 response##data##items,
               )
               |> dispatch;
               ActionShowProgress |> dispatch;
             | _ =>
               response##data##status |> statusModule |> barShowRestoreAction;
               ActionShowProgress |> dispatch;
             }
           )
           |> resolve
         })
      |> catch(error => error |> Js.log |> resolve)
      |> ignore
    );

  let clickFormBoard =
    useCallback(id => {
      ActionShowProgress |> dispatch;
      id |> sViewAJax;
    });

  let sItemAJax = (index, id) =>
    Js.Promise.(
      "newid"
      |> Locals.select
      |> dFormData(id)
      |> Default.sItem
      |> then_(response => {
           (
             switch (response##data##status) {
             | "istrue" =>
               ShowAnimationFull(
                 index,
                 id,
                 response##data##tile,
                 response##data##items,
               )
               |> dispatch;
               ActionShowProgress |> dispatch;
             | _ =>
               response##data##status |> statusModule |> barShowRestoreAction;
               ActionShowProgress |> dispatch;
             }
           )
           |> resolve
         })
      |> catch(error => error |> Js.log |> resolve)
      |> ignore
    );

  let editForm =
    useCallback((i, id, event) => {
      ReactEvent.Mouse.preventDefault(event);
      ReactEvent.Mouse.stopPropagation(event);
      ActionShowProgress |> dispatch;
      id |> sItemAJax(i);
    });

  let deleteAJax = id =>
    Js.Promise.(
      "newid"
      |> Locals.select
      |> dFormData(id)
      |> Default.delete
      |> then_(response =>
           {
             switch (response##data##status) {
             | "istrue" =>
               ClearForm(id) |> dispatch;
               "deleteSuccess" |> statusModule |> barShowRestoreAction;
               ActionShowProgress |> dispatch;
             | _ =>
               response##data##status |> statusModule |> barShowRestoreAction;
               ActionShowProgress |> dispatch;
             };
           }
           |> resolve
         )
      |> catch(error => error |> Js.log |> resolve)
      |> ignore
    );

  let deleteForm =
    useCallback((id, event) => {
      ReactEvent.Mouse.preventDefault(event);
      ReactEvent.Mouse.stopPropagation(event);
      ActionShowProgress |> dispatch;
      id |> deleteAJax;
    });

  let viewPrevious =
    useCallback((id, index, event) => {
      ReactEvent.Mouse.preventDefault(event);
      ReactEvent.Mouse.stopPropagation(event);
      let length = Js_array.length(state.viewitems[index].viewections) - 1;
      let collIndex = id == 0 ? length : id - 1;
      SettingViewCollection(collIndex, index) |> dispatch;
    });

  let viewNext =
    useCallback((id, index, event) => {
      ReactEvent.Mouse.preventDefault(event);
      ReactEvent.Mouse.stopPropagation(event);
      let length = Js_array.length(state.viewitems[index].viewections) - 1;
      let collIndex = id == length ? 0 : id + 1;
      SettingViewCollection(collIndex, index) |> dispatch;
    });

  let sRefreshAJax = () =>
    Js.Promise.(
      "newid"
      |> Locals.select
      |> dFormData(state.formId)
      |> Default.sRefresh
      |> then_(response =>
           {
             switch (response##data##status) {
             | "istrue" =>
               SettingSingleItem(state.formIndex, response##data##items)
               |> dispatch;
               CloseAnimationFull |> dispatch;
               "saveSuccess" |> statusModule |> barShowRestoreAction;
               ActionShowProgress |> dispatch;
             | _ =>
               CloseAnimationFull |> dispatch;
               response##data##status |> statusModule |> barShowRestoreAction;
               ActionShowProgress |> dispatch;
             };
           }
           |> resolve
         )
      |> catch(error => error |> Js.log |> resolve)
      |> ignore
    );

  let insertAJax = () =>
    Js.Promise.(
      "newid"
      |> Locals.select
      |> iFormData(
           state.formId,
           "",
           "",
           Js_array.filter(
             (formitem: formitem) => formitem.formModify === true,
             state.formitems,
           ),
         )
      |> Default.insert
      |> then_(response =>
           {
             switch (response##data##status) {
             | "istrue" => sRefreshAJax()
             | _ =>
               response##data##status |> statusModule |> barShowRestoreAction;
               ActionShowProgress |> dispatch;
             };
           }
           |> resolve
         )
      |> catch(error => error |> Js.log |> resolve)
      |> ignore
    );

  let insertForm =
    useCallback(_ => {
      ActionShowProgress |> dispatch;
      insertAJax();
    });

  let dragOver =
    useCallback((event, i) => {
      ReactEvent.Mouse.preventDefault(event);
      ReactEvent.Mouse.stopPropagation(event);
      ShowDrop(true, i) |> dispatch;
    });

  let dragLeave =
    useCallback((event, i) => {
      ReactEvent.Mouse.preventDefault(event);
      ReactEvent.Mouse.stopPropagation(event);
      ShowDrop(false, i) |> dispatch;
    });

  let uploadAJax = (files, i) => {
    let formData = FormData.make();
    FormData.append(formData, "file", files) |> ignore;
    Js.Promise.(
      formData
      |> Files.upload
      |> then_(response =>
           {
             switch (response##data##status) {
             | "istrue" =>
               ShowFile(
                 response##data##images,
                 response##data##videos,
                 response##data##audios,
                 response##data##files,
                 i,
               )
               |> dispatch;
               ActionShowProgress |> dispatch;
             | _ => ActionShowProgress |> dispatch
             };
           }
           |> resolve
         )
      |> catch(error => error |> Js.log |> resolve)
      |> ignore
    );
  };

  let dropFile =
    useCallback((event, value, i) => {
      ReactEvent.Mouse.preventDefault(event);
      ReactEvent.Mouse.stopPropagation(event);
      ActionShowProgress |> dispatch;
      ShowDrop(false, i) |> dispatch;
      i |> uploadAJax(value);
    });

  let uploadFile =
    useCallback((value, i) => {
      ActionShowProgress |> dispatch;
      i |> uploadAJax(value);
    });

  let uploadsAJax = (files, i) => {
    let formData = FormData.make();
    FormData.append(formData, "file", files) |> ignore;
    Js.Promise.(
      formData
      |> Files.upload
      |> then_(response =>
           {
             switch (response##data##status) {
             | "istrue" =>
               ShowFiles(
                 response##data##images,
                 response##data##videos,
                 response##data##audios,
                 response##data##files,
                 i,
               )
               |> dispatch;
               ActionShowProgress |> dispatch;
             | _ => ActionShowProgress |> dispatch
             };
           }
           |> resolve
         )
      |> catch(error => error |> Js.log |> resolve)
      |> ignore
    );
  };

  let dropFiles =
    useCallback((event, value, i) => {
      ReactEvent.Mouse.preventDefault(event);
      ReactEvent.Mouse.stopPropagation(event);
      ActionShowProgress |> dispatch;
      ShowDrop(false, i) |> dispatch;
      i |> uploadsAJax(value);
    });

  let uploadFiles =
    useCallback((value, i) => {
      ActionShowProgress |> dispatch;
      i |> uploadsAJax(value);
    });

  let chooseFile =
    useCallback(_
      //Documents.GetElementById.make("uploadFile") |> Action.click)
      =>
        switch (fileRef |> Ref.current |> Js.Nullable.toOption) {
        | None => ()
        | Some(el) => el->ReactDOMRe.domElementToObj##click() |> ignore
        }
      );

  let showPrevious =
    useCallback((id, index, event) => {
      ReactEvent.Mouse.preventDefault(event);
      ReactEvent.Mouse.stopPropagation(event);
      let length = Js_array.length(state.formitems[index].collitems) - 1;
      let collIndex = id == 0 ? length : id - 1;
      SettingCollection(collIndex, index) |> dispatch;
    });

  let showNext =
    useCallback((id, index, event) => {
      ReactEvent.Mouse.preventDefault(event);
      ReactEvent.Mouse.stopPropagation(event);
      let length = Js_array.length(state.formitems[index].collitems) - 1;
      let collIndex = id == length ? 0 : id + 1;
      SettingCollection(collIndex, index) |> dispatch;
    });

  let actionImage =
    useCallback((cindex, index, event) => {
      ReactEvent.Mouse.preventDefault(event);
      ReactEvent.Mouse.stopPropagation(event);
      ActionCollection(cindex, index) |> dispatch;
    });

  let changeItem =
    useCallback((value, i) => ChangeItem(value, i) |> dispatch);

  let showMenuItem = useCallback(i => ShowMenuItem(i) |> dispatch);

  let clickMenuItem =
    useCallback((value, i) => ClickMenuItem(value, i) |> dispatch);

  let clickElementItem =
    useCallback((value, ri, i) =>
      switch (value) {
      | "checkbox" => ClickCheckboxItem(ri, i) |> dispatch
      | _ => ClickRadioItem(ri, i) |> dispatch
      }
    );

  let closeAnimationViewFull =
    useCallback(_ => CloseAnimationViewFull |> dispatch);

  let closeAnimationFull = useCallback(_ => CloseAnimationFull |> dispatch);

  <>
    <NewFacetube showProgress={state.showProgress} error={state.error}>
      <GridItem
        style=marginAuto top="0" right="32" bottom="0" left="32" xs="12">
        <GridContainer direction="column" justify="center" alignItem="stretch">
          <GridItem
            style={ReactDOMRe.Style.make(
              ~position="sticky",
              ~top="0px",
              ~zIndex="1",
              (),
            )}
            bottom="0"
            xs="auto">
            <GridContainer direction="row" justify="center" alignItem="center">
              {state.filtitems
               |> Array.mapi((i, filtitem) =>
                    <GridItem top="0" right="0" bottom="0" left="0" xs="auto">
                      <SelectOutline
                        labelColor="rgba(255,0,0,0.8)"
                        tile={filtitem.filtTile}
                        enterBorderColor="rgba(255,0,0,0.8)"
                        downBorderColor="rgba(255,0,0,0.6)"
                        borderColor="rgba(0,0,0,0.2)"
                        value={filtitem.filtValue}
                        disabled={state.showProgress}
                        onClick={_ => i |> showFiltMenu}>
                        ...(
                             filtitem.filtMenu
                               ? <SelectMenu
                                   top="0%"
                                   transform="translate(0, 0%)"
                                   maxHeight="280"
                                   minHeight="0"
                                   topLeft="12"
                                   topRight="12"
                                   bottomRight="12"
                                   bottomLeft="12"
                                   paddingRight="8"
                                   paddingLeft="8">
                                   {filtitem.filtOptions
                                    |> Array.map(filtOption =>
                                         <MenuItem
                                           top="0"
                                           right="8"
                                           bottom="0"
                                           left="8"
                                           disablePadding={
                                                            filtOption.
                                                              optionPadding
                                                          }
                                           topLeft="12"
                                           topRight="12"
                                           bottomRight="12"
                                           bottomLeft="12"
                                           onClick={_ =>
                                             i
                                             |> clickFiltMenu(
                                                  filtOption.value,
                                                  filtitem.filtIndex,
                                                  filtitem.filtOutValue,
                                                )
                                           }>
                                           {filtOption.value |> string}
                                         </MenuItem>
                                       )
                                    |> array}
                                 </SelectMenu>
                               : null,
                             <IconGeneral
                               animation={filtitem.filtMenu |> topDownRorate}
                               src=arrowDownBlack
                             />,
                           )
                      </SelectOutline>
                      <BackgroundBoard
                        showBackground={filtitem.filtMenu}
                        backgroundColor="transparent"
                        onClick={_ => i |> showFiltMenu}
                      />
                    </GridItem>
                  )
               |> array}
            </GridContainer>
          </GridItem>
          <GridItem right="24" bottom="0" left="24" xs="auto">
            <GridContainer direction="row" justify="start" alignItem="center">
              {state.items
               |> Array.mapi((i, item) =>
                    <div onClick={_ => item.id |> clickFormBoard}>
                      <GridItem
                        style={ReactDOMRe.Style.make(
                          ~height="250px",
                          ~marginRight="12px",
                          (),
                        )}
                        top="0"
                        right="0"
                        bottom="0"
                        left="0"
                        width="276px"
                        cursor="pointer"
                        enterBorderWidth="2"
                        borderWidth="2"
                        enterBorderColor="rgba(255,0,0,0.8)"
                        enterBorderRadius="4"
                        borderRadius="1"
                        xs="no">
                        <Card>
                          <GridContainer
                            direction="column"
                            justify="center"
                            alignItem="stretch">
                            <GridItem
                              style={ReactDOMRe.Style.combine(
                                positionRelative,
                                ReactDOMRe.Style.make(~height="159px", ()),
                              )}
                              top="0"
                              right="0"
                              bottom="0"
                              left="0"
                              width="276px"
                              xs="no">
                              <GridContainer
                                direction="row"
                                justify="center"
                                alignItem="center">
                                <GridItem
                                  top="0"
                                  right="0"
                                  bottom="0"
                                  left="0"
                                  xs="no">
                                  <div
                                    style={ReactDOMRe.Style.combine(
                                      outsideCollections,
                                      ReactDOMRe.Style.make(~left="0", ()),
                                    )}>
                                    <IconButton
                                      padding="6"
                                      disabled={state.showProgress}
                                      onClick={event =>
                                        event
                                        |> showPreviousCollections(
                                             item.index,
                                             i,
                                           )
                                      }>
                                      <IconAction
                                        animation="leftRight"
                                        src=arrowBackIosBlack
                                      />
                                    </IconButton>
                                  </div>
                                </GridItem>
                                <GridItem
                                  style={ReactDOMRe.Style.make(
                                    ~height="155px",
                                    (),
                                  )}
                                  top="0"
                                  right="0"
                                  bottom="0"
                                  left="0"
                                  xs="auto">
                                  {item.collections
                                   |> Array.mapi((ci, collitem) =>
                                        <MediaImage
                                          showImage={item.index == ci}
                                          style={ReactDOMRe.Style.make(
                                            ~position="absolute",
                                            ~height="155px",
                                            ~left="50%",
                                            ~transform="translate(-50%, 0)",
                                            (),
                                          )}
                                          width="auto"
                                          height="100%"
                                          src={
                                            "data:image/jpg;base64,"
                                            ++ collitem
                                          }
                                        />
                                      )
                                   |> array}
                                </GridItem>
                                <GridItem
                                  top="0"
                                  right="0"
                                  bottom="0"
                                  left="0"
                                  xs="no">
                                  <div
                                    style={ReactDOMRe.Style.combine(
                                      outsideCollections,
                                      ReactDOMRe.Style.make(~right="0", ()),
                                    )}>
                                    <IconButton
                                      padding="6"
                                      disabled={state.showProgress}
                                      onClick={event =>
                                        event
                                        |> showNextCollections(item.index, i)
                                      }>
                                      <IconAction
                                        animation="leftRight"
                                        src=arrowForwardIosBlack
                                      />
                                    </IconButton>
                                  </div>
                                </GridItem>
                              </GridContainer>
                            </GridItem>
                            <GridItem bottom="0" left="16" xs="auto">
                              <GridContainer
                                direction="row"
                                justify="center"
                                alignItem="start">
                                <GridItem
                                  top="0"
                                  right="0"
                                  bottom="0"
                                  left="0"
                                  xs="no">
                                  <Avatar
                                    top="0"
                                    right="12"
                                    bottom="0"
                                    left="0"
                                    color="#909090"
                                    enterBorderColor="transparent"
                                    downBorderColor="transparent"
                                    backgroundColor="rgba(0,0,0,0.08)">
                                    {item.creator |> string}
                                  </Avatar>
                                </GridItem>
                                <GridItem
                                  top="0"
                                  right="0"
                                  bottom="0"
                                  left="0"
                                  xs="auto">
                                  <GridContainer
                                    direction="column"
                                    justify="center"
                                    alignItem="stretch">
                                    <GridItem
                                      top="0"
                                      right="0"
                                      bottom="3"
                                      left="0"
                                      xs="auto">
                                      <Typography
                                        variant="subheading" noWrap=true>
                                        {item.tile |> string}
                                      </Typography>
                                    </GridItem>
                                    <GridItem
                                      top="0"
                                      right="0"
                                      bottom="0"
                                      left="0"
                                      xs="auto">
                                      <Typography
                                        variant="caption" color="#606060">
                                        {item.datetime |> string}
                                      </Typography>
                                    </GridItem>
                                    <GridItem
                                      style=positionRelative
                                      top="0"
                                      right="0"
                                      bottom="0"
                                      left="0"
                                      xs="auto">
                                      {state.update
                                         ? <div
                                             style={ReactDOMRe.Style.make(
                                               ~position="absolute",
                                               ~right="50%",
                                               ~bottom="-100%",
                                               ~transform=
                                                 "translate(50px, 20px)",
                                               (),
                                             )}>
                                             <IconButton
                                               padding="6"
                                               disabled={state.showProgress}
                                               onClick={event =>
                                                 event |> editForm(i, item.id)
                                               }>
                                               <IconAction
                                                 animation="leftRight"
                                                 src=editBlack
                                               />
                                             </IconButton>
                                           </div>
                                         : null}
                                      {state.delete
                                         ? <div
                                             style={ReactDOMRe.Style.make(
                                               ~position="absolute",
                                               ~right="0",
                                               ~bottom="-100%",
                                               ~transform=
                                                 "translate(0px, 20px)",
                                               (),
                                             )}>
                                             <IconButton
                                               padding="6"
                                               disabled={state.showProgress}
                                               onClick={event =>
                                                 event |> deleteForm(item.id)
                                               }>
                                               <IconAction
                                                 animation="leftRight"
                                                 src=deleteBlack
                                               />
                                             </IconButton>
                                           </div>
                                         : null}
                                    </GridItem>
                                  </GridContainer>
                                </GridItem>
                              </GridContainer>
                            </GridItem>
                          </GridContainer>
                        </Card>
                      </GridItem>
                    </div>
                  )
               |> array}
            </GridContainer>
          </GridItem>
        </GridContainer>
        <BottomScroll
          showBar={state.showItem}
          disabled={state.showProgress}
          onClick=clickScrollBar
        />
      </GridItem>
    </NewFacetube>
    <DialogFull showAnimation={state.viewFull}>
      <DialogTitle top="22" left="64">
        <Typography variant="tile" fontWeight="600">
          {state.viewTitle |> string}
        </Typography>
      </DialogTitle>
      <DialogContent>
        <DialogContentText>
          <GridItem
            style=marginAuto
            top="0"
            right="0"
            bottom="0"
            left="54"
            xs="12"
            maxWidth="1080px">
            <GridContainer
              direction="column" justify="center" alignItem="stretch">
              {state.viewitems
               |> Array.mapi((i, item) =>
                    <>
                      <GridItem top="0" right="0" left="0" xs="auto">
                        <GridContainer
                          direction="row" justify="between" alignItem="start">
                          <GridItem
                            style={ReactDOMRe.Style.combine(
                              marginAuto,
                              ReactDOMRe.Style.make(
                                ~position={
                                  state.formWidth < 907
                                    ? "relative" : "sticky";
                                },
                                ~top="0px",
                                (),
                              ),
                            )}
                            top="36"
                            right="56"
                            bottom="0"
                            left="0"
                            width="100%"
                            maxWidth="450px"
                            xs="no">
                            <GridContainer
                              direction="row"
                              justify="center"
                              alignItem="center">
                              <GridItem
                                top="0" right="0" bottom="0" left="0" xs="no">
                                <IconButton
                                  padding="6"
                                  disabled={state.showProgress}
                                  onClick={event =>
                                    event |> viewPrevious(item.viewIndex, i)
                                  }>
                                  <IconAction
                                    animation="leftRight"
                                    src=arrowBackIosBlack
                                  />
                                </IconButton>
                              </GridItem>
                              <GridItem
                                style=positionRelative
                                top="0"
                                right="0"
                                bottom="0"
                                left="0"
                                xs="auto">
                                {item.viewections
                                 |> Array.mapi((vi, viewitem) =>
                                      <MediaImage
                                        showImage={item.viewIndex == vi}
                                        width="100%"
                                        height="auto"
                                        src={
                                          "data:image/jpg;base64," ++ viewitem
                                        }
                                      />
                                    )
                                 |> array}
                              </GridItem>
                              <GridItem
                                top="0" right="0" bottom="0" left="0" xs="no">
                                <IconButton
                                  padding="6"
                                  disabled={state.showProgress}
                                  onClick={event =>
                                    event |> viewNext(item.viewIndex, i)
                                  }>
                                  <IconAction
                                    animation="leftRight"
                                    src=arrowForwardIosBlack
                                  />
                                </IconButton>
                              </GridItem>
                            </GridContainer>
                          </GridItem>
                          <GridItem
                            top="36" right="56" bottom="0" left="0" xs="auto">
                            <GridContainer
                              direction="column"
                              justify="center"
                              alignItem="stretch">
                              {item.dataitems
                               |> Array.map(dataitem =>
                                    <>
                                      <GridItem right="40" left="40" xs="auto">
                                        <Typography
                                          variant="tile"
                                          fontSize="1.25rem"
                                          fontWeight="bolder"
                                          noWrap=true>
                                          {dataitem.key |> string}
                                        </Typography>
                                      </GridItem>
                                      <GridItem
                                        top="6"
                                        right="40"
                                        left="40"
                                        bottom="6"
                                        xs="auto">
                                        <Typography
                                          variant="subheading"
                                          fontWeight="500"
                                          noWrap=true>
                                          {dataitem.data |> string}
                                        </Typography>
                                      </GridItem>
                                      <GridItem right="40" left="40" xs="auto">
                                        <Divider />
                                      </GridItem>
                                    </>
                                  )
                               |> array}
                            </GridContainer>
                          </GridItem>
                        </GridContainer>
                      </GridItem>
                    </>
                  )
               |> array}
            </GridContainer>
          </GridItem>
        </DialogContentText>
      </DialogContent>
      <DialogActions>
        <div
          style={ReactDOMRe.Style.make(
            ~position="fixed",
            ~top="10px",
            ~left="10px",
            (),
          )}>
          <IconButton
            padding="12"
            disabled={state.showProgress}
            onClick=closeAnimationViewFull>
            <Tooltip location="bottom" backgroundColor="rgba(255,0,0,0.8)">
              <FormattedMessage id="closed" defaultMessage="Closed" />
            </Tooltip>
            <IconAction animation="circle" src=clearBlack />
          </IconButton>
        </div>
      </DialogActions>
    </DialogFull>
    <DialogFull showAnimation={state.showFull}>
      <DialogTitle top="22" left="64">
        <Typography variant="tile" fontWeight="600">
          {state.formTitle |> string}
        </Typography>
      </DialogTitle>
      <DialogContent>
        <DialogContentText>
          <GridItem
            style=marginAuto
            top="0"
            right="0"
            bottom="0"
            left="54"
            xs="12"
            maxWidth="770px">
            <GridContainer
              direction="column" justify="center" alignItem="stretch">
              <GridItem
                style={ReactDOMRe.Style.make(
                  ~position="sticky",
                  ~top="0px",
                  ~zIndex="1000",
                  (),
                )}
                top="0"
                bottom="6"
                left="0"
                xs="auto">
                <GridContainer
                  direction="row" justify="around" alignItem="center">
                  <GridItem top="0" right="0" bottom="0" left="0" xs="auto">
                    null
                  </GridItem>
                  <GridItem top="0" right="0" bottom="0" left="0" xs="no">
                    <Button disabled={state.showProgress} onClick=insertForm>
                      <IconAction animation="leftRight" src=saveWhite />
                      <FormattedMessage id="save" defaultMessage="Save" />
                    </Button>
                  </GridItem>
                </GridContainer>
              </GridItem>
              {state.formitems
               |> Array.mapi((i, item) =>
                    <>
                      <GridItem right="0" left="0" xs="auto">
                        <GridContainer
                          direction="column"
                          justify="start"
                          alignItem="stretch">
                          <GridItem
                            top="0" right="20" bottom="0" left="20" xs="auto">
                            <Typography
                              variant="subheading"
                              fontSize="1.2rem"
                              fontWeight="bolder">
                              {item.title |> string}
                            </Typography>
                          </GridItem>
                          <GridItem top="6" bottom="0" xs="auto">
                            {switch (item.outValue) {
                             | "label" =>
                               <Typography variant="subtitle2" noWrap=true>
                                 {item.values |> string}
                               </Typography>
                             | "image" =>
                               <ImageUpload
                                 webLoad={state.showProgress}
                                 showDrop={item.showDrop}
                                 showFile={item.showFile}
                                 src={item.values}
                                 fileRef
                                 onDragOver={event => i |> dragOver(event)}
                                 onDragLeave={event => i |> dragLeave(event)}
                                 onDrop={event =>
                                   i
                                   |> dropFile(
                                        event,
                                        ReactEvent.Synthetic.nativeEvent(
                                          event,
                                        )##dataTransfer##files[0],
                                      )
                                 }
                                 disabled={state.showProgress}
                                 onClick=chooseFile
                                 onChange={event =>
                                   i
                                   |> uploadFile(
                                        ReactEvent.Form.target(event)##files[0],
                                      )
                                 }
                               />
                             | "collections" =>
                               <CollectionUpload
                                 webLoad={state.showProgress}
                                 showDrop={item.showDrop}
                                 showFile={item.showFile}
                                 fileRef
                                 onDragOver={event => i |> dragOver(event)}
                                 onDragLeave={event => i |> dragLeave(event)}
                                 onDrop={event =>
                                   i
                                   |> dropFiles(
                                        event,
                                        ReactEvent.Synthetic.nativeEvent(
                                          event,
                                        )##dataTransfer##files[0],
                                      )
                                 }
                                 disabled={state.showProgress}
                                 onClick=chooseFile
                                 onChange={event =>
                                   i
                                   |> uploadFiles(
                                        ReactEvent.Form.target(event)##files[0],
                                      )
                                 }
                                 showPrevious={event =>
                                   event |> showPrevious(item.collIndex, i)
                                 }
                                 showNext={event =>
                                   event |> showNext(item.collIndex, i)
                                 }>
                                 {item.collitems
                                  |> Array.mapi((ci, collitem) =>
                                       <>
                                         <MediaImage
                                           showImage={item.collIndex == ci}
                                           src={
                                             "data:image/jpg;base64,"
                                             ++ collitem.value
                                           }
                                         />
                                         {switch (
                                            item.collIndex == ci,
                                            collitem.showDelete,
                                          ) {
                                          | (true, true) =>
                                            <div
                                              style={ReactDOMRe.Style.make(
                                                ~position="absolute",
                                                ~top="20px",
                                                ~right="20px",
                                                ~transform=
                                                  "translate(0px, 0%)",
                                                ~zIndex="1",
                                                (),
                                              )}>
                                              <IconButton
                                                padding="6"
                                                disabled={state.showProgress}
                                                onClick={event =>
                                                  event
                                                  |> actionImage(
                                                       item.collIndex,
                                                       i,
                                                     )
                                                }>
                                                <IconAction
                                                  animation="leftRight"
                                                  src={
                                                    collitem.collDelete
                                                      ? refreshBlack
                                                      : clearWarn
                                                  }
                                                />
                                              </IconButton>
                                            </div>
                                          | (_, _) => null
                                          }}
                                       </>
                                     )
                                  |> array}
                               </CollectionUpload>
                             | "text" =>
                               <TextFieldStandard
                                 width="50"
                                 top="0"
                                 left="0"
                                 borderTop="10"
                                 borderBottom="10"
                                 enterBorderColor="rgba(255,0,0,0.8)"
                                 downBorderColor="rgba(255,0,0,0.6)"
                                 borderColor="rgba(0,0,0,0.2)"
                                 value={item.values}
                                 disabled={state.showProgress}
                                 onChange={event =>
                                   i
                                   |> changeItem(
                                        ReactEvent.Form.target(event)##value,
                                      )
                                 }>
                                 null
                               </TextFieldStandard>
                             | "textline" =>
                               <TextFieldStandard
                                 top="0"
                                 left="0"
                                 borderTop="10"
                                 borderBottom="10"
                                 enterBorderColor="rgba(255,0,0,0.8)"
                                 downBorderColor="rgba(255,0,0,0.6)"
                                 borderColor="rgba(0,0,0,0.2)"
                                 value={item.values}
                                 disabled={state.showProgress}
                                 onChange={event =>
                                   i
                                   |> changeItem(
                                        ReactEvent.Form.target(event)##value,
                                      )
                                 }>
                                 null
                               </TextFieldStandard>
                             | "textarea" =>
                               <TextFieldMultiline
                                 top="0"
                                 bottom="0"
                                 left="0"
                                 labelColor="rgba(255,0,0,0.8)"
                                 borderTop="10"
                                 borderBottom="10"
                                 enterBorderColor="rgba(255,0,0,0.8)"
                                 downBorderColor="rgba(255,0,0,0.6)"
                                 borderColor="rgba(0,0,0,0.2)"
                                 rows=3
                                 value={item.values}
                                 disabled={state.showProgress}
                                 onChange={event =>
                                   i
                                   |> changeItem(
                                        ReactEvent.Form.target(event)##value,
                                      )
                                 }>
                                 null
                               </TextFieldMultiline>
                             | "droplist" =>
                               <>
                                 <SelectStandard
                                   top="0"
                                   left="0"
                                   enterBorderColor="rgba(255,0,0,0.8)"
                                   downBorderColor="rgba(255,0,0,0.6)"
                                   borderColor="rgba(0,0,0,0.2)"
                                   value={item.values}
                                   disabled={state.showProgress}
                                   onClick={_ => i |> showMenuItem}>
                                   ...(
                                        item.showMenu
                                          ? <SelectMenu
                                              top="50%"
                                              transform="translate(0, -50%)"
                                              maxHeight="280"
                                              minHeight="0"
                                              topLeft="12"
                                              topRight="12"
                                              bottomRight="12"
                                              bottomLeft="12"
                                              paddingRight="8"
                                              paddingLeft="8">
                                              {item.optionitems
                                               |> Array.map(optionitem =>
                                                    <MenuItem
                                                      top="0"
                                                      right="8"
                                                      bottom="0"
                                                      left="8"
                                                      disablePadding={
                                                                    optionitem.
                                                                    optionPadding
                                                                    }
                                                      topLeft="12"
                                                      topRight="12"
                                                      bottomRight="12"
                                                      bottomLeft="12"
                                                      onClick={_ =>
                                                        i
                                                        |> clickMenuItem(
                                                             optionitem.value,
                                                           )
                                                      }>
                                                      {optionitem.value
                                                       |> string}
                                                    </MenuItem>
                                                  )
                                               |> array}
                                            </SelectMenu>
                                          : null,
                                        <IconGeneral
                                          animation={
                                            item.showMenu |> topDownRorate
                                          }
                                          src=arrowDownBlack
                                        />,
                                      )
                                 </SelectStandard>
                                 <BackgroundBoard
                                   showBackground={item.showMenu}
                                   backgroundColor="transparent"
                                   onClick={_ => i |> showMenuItem}
                                 />
                               </>
                             | _ =>
                               <GridContainer
                                 direction="column"
                                 justify="center"
                                 alignItem="stretch">
                                 {item.answeritems
                                  |> Array.mapi((ai, answeritem) =>
                                       <GridItem
                                         top="0"
                                         bottom="6"
                                         left="0"
                                         right="0"
                                         xs="auto">
                                         <GridContainer
                                           direction="row"
                                           justify="start"
                                           alignItem="center">
                                           <GridItem
                                             top="0"
                                             right="0"
                                             bottom="0"
                                             left="0"
                                             xs="no">
                                             <IconButton
                                               padding="4"
                                               disabled={state.showProgress}>
                                               <IconAction
                                                 animation="leftRight"
                                                 src=radioButtonUncheckedBlack
                                               />
                                             </IconButton>
                                           </GridItem>
                                           <GridItem
                                             top="0"
                                             right="6"
                                             bottom="0"
                                             left="0"
                                             xs="auto">
                                             <TextFieldStandard
                                               top="0"
                                               enterBorderColor={
                                                 answeritem.showAnswer
                                                 |> enterBorder
                                               }
                                               downBorderColor={
                                                 answeritem.showAnswer
                                                 |> downBorder
                                               }
                                               borderColor={
                                                 answeritem.showAnswer
                                                 |> border
                                               }
                                               placeholder="Option"
                                               value={answeritem.value}
                                               disabled=true>
                                               null
                                             </TextFieldStandard>
                                           </GridItem>
                                           <GridItem
                                             top="0"
                                             right="6"
                                             bottom="0"
                                             left="0"
                                             xs="no">
                                             <IconButton
                                               padding="4"
                                               disabled={state.showProgress}
                                               onClick={_ =>
                                                 i
                                                 |> clickElementItem(
                                                      item.outValue,
                                                      ai,
                                                    )
                                               }>
                                               <IconAction
                                                 animation="leftRight"
                                                 src={
                                                   answeritem.showAnswer
                                                     ? doneSuccessful
                                                     : errorWarn
                                                 }
                                               />
                                             </IconButton>
                                           </GridItem>
                                         </GridContainer>
                                       </GridItem>
                                     )
                                  |> array}
                               </GridContainer>
                             }}
                          </GridItem>
                        </GridContainer>
                      </GridItem>
                      <GridItem xs="auto"> <Divider /> </GridItem>
                    </>
                  )
               |> array}
            </GridContainer>
          </GridItem>
        </DialogContentText>
      </DialogContent>
      <DialogActions>
        <div
          style={ReactDOMRe.Style.make(
            ~position="fixed",
            ~top="10px",
            ~left="10px",
            (),
          )}>
          <IconButton
            padding="12"
            disabled={state.showProgress}
            onClick=closeAnimationFull>
            <Tooltip location="bottom" backgroundColor="rgba(255,0,0,0.8)">
              <FormattedMessage id="closed" defaultMessage="Closed" />
            </Tooltip>
            <IconAction animation="circle" src=clearBlack />
          </IconButton>
        </div>
      </DialogActions>
    </DialogFull>
    <SnackbarYoutube showYoutube={state.showYoutube} position="bottomLeft">
      ...(<span> {state.youtubeText |> string} </span>, null)
    </SnackbarYoutube>
  </>;
};
