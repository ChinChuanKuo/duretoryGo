open React;
open Together;
open ReactIntl;
open Icons;
open Data;
open Items;
open Axiosapi;
open Status;
open Path;
open Storage;
open AnswerColor;
open IconAnimation;
open SelectPosition;
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
  collInsert: bool,
  collDelete: bool,
};

type item = {
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
};

let newcollitem = (id, showImage, showVideo, showAudio, value) => [|
  {
    id,
    showImage,
    showVideo,
    showAudio,
    value,
    collInsert: true,
    collDelete: false,
  },
|];

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
  items: array(item),
  showYoutube: bool,
  youtubeText: string,
};

type action =
  | SettingError
  | SettingFormLoad
  | SettingFormWidth(int, int)
  | ActionShowProgress
  | ActionPermissItems(bool, bool, bool, bool)
  | SettingFormItems(array(item))
  | ShowDrop(bool, int)
  | ShowFile(bool, bool, bool, string, int)
  | ShowFiles(bool, bool, bool, string, int)
  | SettingCollection(int, int)
  | ChangeItem(string, int)
  | ShowMenuItem(int)
  | ClickMenuItem(string, int)
  | ClickRadioItem(int, int)
  | ClickCheckboxItem(int, int)
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
  | SettingFormItems(items) => {...state, items}
  | ShowDrop(droped, index) => {
      ...state,
      items:
        Array.mapi(
          (i, item) => index == i ? {...item, showDrop: droped} : item,
          state.items,
        ),
    }
  | ShowFile(showImage, showVideo, showAudio, values, index) => {
      ...state,
      items:
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
              }
              : item,
          state.items,
        ),
    }
  | ShowFiles(showImage, showVideo, showAudio, values, index) => {
      ...state,
      items:
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
              }
              : item,
          state.items,
        ),
    }
  | SettingCollection(collIndex, index) => {
      ...state,
      items:
        Array.mapi(
          (i, item) => index == i ? {...item, collIndex} : item,
          state.items,
        ),
    }
  | ChangeItem(value, index) => {
      ...state,
      items:
        Array.mapi(
          (i, item) => index == i ? {...item, values: value} : item,
          state.items,
        ),
    }
  | ShowMenuItem(index) => {
      ...state,
      items:
        Array.mapi(
          (i, item) =>
            index == i ? {...item, showMenu: !item.showMenu} : item,
          state.items,
        ),
    }
  | ClickMenuItem(value, index) => {
      ...state,
      items:
        Array.mapi(
          (i, item) =>
            index == i
              ? {...item, values: value, showMenu: !item.showMenu} : item,
          state.items,
        ),
    }
  | ClickRadioItem(rindex, index) => {
      ...state,
      items:
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
              }
              : item,
          state.items,
        ),
    }
  | ClickCheckboxItem(rindex, index) => {
      ...state,
      items:
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
              }
              : item,
          state.items,
        ),
    }
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
  items: [||],
  showYoutube: false,
  youtubeText: "",
};

let positionRelative = ReactDOMRe.Style.make(~position="relative", ());

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

  let searchAJax = () =>
    Js.Promise.(
      "newid"
      |> Locals.select
      |> userData
      |> Axiosapi.Create.search
      |> then_(response =>
           {
             switch (response##data##status) {
             | "istrue" =>
               SettingFormItems(response##data##items) |> dispatch;
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
    let sizeId = Window.Listeners.add("resize", handleResize, true) |> ignore;
    /*let scrollId =
      Window.Listeners.add("scroll", handleScrollBar, true) |> ignore;*/
    Some(() => sizeId);
  });

  let insertAJax = () =>
    Js.Promise.(
      "newid"
      |> Locals.select
      |> iItemsData(state.items)
      |> Axiosapi.Create.insert
      |> then_(response =>
           {
             switch (response##data##status) {
             | "istrue" =>
               "saveSuccess" |> Sessions.create("form");
               homePath |> ReasonReactRouter.push;
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
      let length = Js_array.length(state.items[index].collitems) - 1;
      let collIndex = id == 0 ? length : id - 1;
      SettingCollection(collIndex, index) |> dispatch;
    });

  let showNext =
    useCallback((id, index, event) => {
      ReactEvent.Mouse.preventDefault(event);
      ReactEvent.Mouse.stopPropagation(event);
      let length = Js_array.length(state.items[index].collitems) - 1;
      let collIndex = id == length ? 0 : id + 1;
      SettingCollection(collIndex, index) |> dispatch;
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

  <>
    <NewFacetube showProgress={state.showProgress} error={state.error}>
      <GridItem
        style=marginAuto
        top="0"
        right="32"
        bottom="0"
        left="32"
        xs="12"
        maxWidth="770px">
        <GridContainer direction="column" justify="center" alignItem="stretch">
          <GridItem
            style={ReactDOMRe.Style.make(
              ~position="sticky",
              ~top="0px",
              ~zIndex="1000",
              (),
            )}
            top="0"
            right="24"
            left="24"
            xs="auto">
            <GridContainer direction="row" justify="around" alignItem="center">
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
          <GridItem top="0" right="24" left="24" xs="auto">
            <GridContainer
              direction="column" justify="center" alignItem="stretch">
              {state.items
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
                                       <MediaImage
                                         showImage={item.collIndex == ci}
                                         src={
                                           "data:image/jpg;base64,"
                                           ++ collitem.value
                                         }
                                       />
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
                                              top={i |> Position.top}
                                              transform={
                                                i |> Position.transform
                                              }
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
                                                 src={
                                                   answeritem.showAnswer
                                                   |> answerIcon(
                                                        item.outValue,
                                                      )
                                                 }
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
        </GridContainer>
      </GridItem>
    </NewFacetube>
    <SnackbarYoutube showYoutube={state.showYoutube} position="bottomLeft">
      ...(<span> {state.youtubeText |> string} </span>, null)
    </SnackbarYoutube>
  </>;
};
