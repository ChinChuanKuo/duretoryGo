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
open ObjectFormat;
open IconAnimation;
[%bs.raw {|require('../../../scss/pages/Together/together.scss')|}];

type answeritem = {
  id: int,
  value: string,
  showAnswer: bool,
};

type collectitem = {
  id: int,
  collImage: bool,
  collVideo: bool,
  collAudio: bool,
  value: string,
  collInsert: bool,
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
  collectitems: array(collectitem),
  optionitems: array(optionitem),
  answeritems: array(answeritem),
  formModify: bool,
};

type collitem = {
  subId: string,
  value: string,
  creator: string,
};

type item = {
  id: string,
  collection: string,
  collitems: array(collitem),
  attribute: string,
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
  showItem: bool,
  itemCount: int,
  items: array(item),
  showFull: bool,
  formIndex: int,
  formId: string,
  formTitle: string,
  formitems: array(formitem),
  showYoutube: bool,
  youtubeText: string,
};

let newcollectitem = (id, collImage, collVideo, collAudio, value) => [|
  {
    id,
    collImage,
    collVideo,
    collAudio,
    collInsert: true,
    collDelete: false,
    value,
  },
|];

type action =
  | SettingError
  | SettingFormLoad
  | SettingFormWidth(int, int)
  | ActionShowProgress
  | ActionPermissItems(bool, bool, bool, bool)
  | SettingFormItems(bool, int, array(item))
  | SettingScrollItems(bool, array(item))
  | ShowAnimationFull(int, string, string, array(formitem))
  | ClearForm(string)
  | ShowDrop(bool, int)
  | ShowFile(bool, bool, bool, string, int)
  | ShowFiles(bool, bool, bool, string, int)
  | ChangeItem(string, int)
  | ShowMenuItem(int)
  | ClickMenuItem(string, int)
  | ClickRadioItem(int, int)
  | ClickCheckboxItem(int, int)
  | CloseAnimationFull
  | ActionSnackBar(string, bool);

let reducer = (state, action) =>
  switch (action) {
  | SettingError => {...state, error: !state.error}
  | SettingFormLoad => {...state, formLoad: !state.formLoad}
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
  | ShowAnimationFull(index, id, value, formitems) => {
      ...state,
      formIndex: index,
      formId: id,
      formTitle: value,
      formitems,
      showFull: !state.showFull,
    }
  | ClearForm(id) => {
      ...state,
      itemCount: state.itemCount - 1,
      error: state.itemCount == 1,
      items: Js_array.filter((item: item) => item.id !== id, state.items),
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
                collectitems:
                  Array.append(
                    item.collectitems,
                    newcollectitem(
                      Js_array.length(item.collectitems) + 1,
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
  showItem: false,
  itemCount: 0,
  items: [||],
  showFull: false,
  formIndex: 0,
  formId: "",
  formTitle: "",
  formitems: [||],
  showYoutube: false,
  youtubeText: "",
};

[@react.component]
let make = _ => {
  let (state, dispatch) = useReducer(reducer, initialState);

  let fileRef = useRef(Js.Nullable.null);

  let barShowRestoreAction = youtubeText => {
    ActionSnackBar(youtubeText, true) |> dispatch;
    Js.Global.setTimeout(() => ActionSnackBar("", false) |> dispatch, 5000)
    |> ignore;
  };

  let searchAJax = () =>
    Js.Promise.(
      state.items
      |> Js_array.length
      |> string_of_int
      |> otherData("newid" |> Locals.select)
      |> Default.search
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
             searchAJax();
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
      let testtime = SettingFormLoad |> dispatch;
      let sizeId =
        SettingFormWidth(Window.Sizes.width, Window.Sizes.height) |> dispatch;
      let testId =
        switch ("form" |> Sessions.select |> checkObjects) {
        | "saveSuccess"
        | "sendSuccess"
        | "deleteSuccess" =>
          "form"
          |> Sessions.select
          |> checkObjects
          |> statusModule
          |> barShowRestoreAction;
          "" |> Sessions.create("form");
        | _ => "action" |> Js.log
        };
      let timeId = permissAJax();
      Some(
        () => {
          testtime;
          sizeId;
          testId;
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
    let sizeId = Window.Listeners.add("resize", handleResize, true) |> ignore;
    /*let scrollId =
      Window.Listeners.add("scroll", handleScrollBar, true) |> ignore;*/
    Some(() => sizeId);
  });

  let scrollAJax = length =>
    Js.Promise.(
      length
      |> otherData("newid" |> Locals.select)
      |> Default.scroll
      |> then_(response =>
           {
             SettingScrollItems(
               response##data##showItem,
               response##data##items,
             )
             |> dispatch;
             ActionShowProgress |> dispatch;
           }
           |> resolve
         )
      |> catch(error => error |> Js.log |> resolve)
      |> ignore
    );

  let clickScrollBar =
    useCallback(_ => {
      ActionShowProgress |> dispatch;
      state.items |> Js_array.length |> string_of_int |> scrollAJax;
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
    useCallback(id => {
      ActionShowProgress |> dispatch;
      id |> deleteAJax;
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

  let clickFormBoard =
    useCallback((i, id) => {
      ActionShowProgress |> dispatch;
      id |> sItemAJax(i);
    });

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
             | "istrue" =>
               CloseAnimationFull |> dispatch;
               "saveSuccess" |> statusModule |> barShowRestoreAction;
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

  let insertForm =
    useCallback(_ => {
      ActionShowProgress |> dispatch;
      insertAJax();
    });

  let dragOver =
    useCallback((event, i) => {
      ReactEventRe.Mouse.preventDefault(event);
      ReactEventRe.Mouse.stopPropagation(event);
      ShowDrop(true, i) |> dispatch;
    });

  let dragLeave =
    useCallback((event, i) => {
      ReactEventRe.Mouse.preventDefault(event);
      ReactEventRe.Mouse.stopPropagation(event);
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
      ReactEventRe.Mouse.preventDefault(event);
      ReactEventRe.Mouse.stopPropagation(event);
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
      ReactEventRe.Mouse.preventDefault(event);
      ReactEventRe.Mouse.stopPropagation(event);
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

  let closeAnimationFull = useCallback(_ => CloseAnimationFull |> dispatch);

  <>
    <NewFacetube showProgress={state.showProgress} error={state.error}>
      <GridItem
        style=marginAuto top="0" right="32" bottom="0" left="32" xs="12">
        <GridContainer direction="column" justify="center" alignItem="stretch">
          <GridItem right="24" bottom="0" left="24" xs="auto">
            <GridContainer direction="row" justify="start" alignItem="center">
              {state.items
               |> Array.mapi((i, item) =>
                    <div onClick={_ => item.id |> clickFormBoard(i)}>
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
                              style=marginAuto
                              top="0"
                              right="0"
                              bottom="0"
                              left="0"
                              xs="no">
                              <div
                                style={ReactDOMRe.Style.make(
                                  ~height="155px",
                                  (),
                                )}>
                                <Image
                                  width="auto"
                                  height="100%"
                                  borderRadius="6"
                                  src={
                                    "data:image/jpg;base64," ++ item.collection
                                  }
                                />
                              </div>
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
                                        {item.attribute |> string}
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
                                    {item.itemDelete
                                       ? <GridItem
                                           style={ReactDOMRe.Style.make(
                                             ~position="relative",
                                             (),
                                           )}
                                           top="0"
                                           right="0"
                                           bottom="0"
                                           left="0"
                                           xs="auto">
                                           <div
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
                                               onClick={_ =>
                                                 item.id |> deleteForm
                                               }>
                                               <IconAction
                                                 animation="leftRight"
                                                 src=deleteBlack
                                               />
                                             </IconButton>
                                           </div>
                                         </GridItem>
                                       : null}
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
                                        ReactEventRe.Synthetic.nativeEvent(
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
                                   |> dropFiles(
                                        event,
                                        ReactEventRe.Synthetic.nativeEvent(
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
                               />
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
