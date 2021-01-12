open React;
open Together;
open ReactIntl;
open Icons;
open Data;
open Items;
open Axiosapi;
open Path;
open Status;
open Storage;
open SwitchColor;
open IconAnimation;
[%bs.raw {|require('../../../scss/pages/Together/together.scss')|}];

type answeritem = {
  id: int,
  value: string,
  ansrDelete: bool,
};

type item = {
  iid: int,
  showLine: bool,
  title: string,
  showOut: bool,
  showDrop: bool,
  showFile: bool,
  outValue: string,
  showShow: bool,
  showCheck: bool,
  showFilter: bool,
  showMore: bool,
  opticonitems: array(opticonitem),
  answeritems: array(answeritem),
  itemModify: bool,
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
  items: array(item),
  showYoutube: bool,
  youtubeText: string,
};

let newitem = (iid, opticonitems) => [|
  {
    iid,
    showLine: false,
    title: "",
    showOut: false,
    showDrop: false,
    showFile: false,
    outValue: "radio",
    showShow: false,
    showCheck: false,
    showFilter: false,
    showMore: false,
    opticonitems,
    answeritems: [|{id: 1, value: "", ansrDelete: false}|],
    itemModify: false,
    itemDelete: false,
  },
|];

let newansweritem = id => [|{id, value: "", ansrDelete: false}|];

type action =
  | SettingError
  | SettingFormLoad
  | SettingFormWidth(int, int)
  | ActionShowProgress
  | ActionPermissItems(bool, bool, bool, bool)
  | SettingFormItems(array(item))
  | AddForm(array(opticonitem))
  | ClickBoardPaper(int)
  | ChangeTitle(string, int)
  | ShowOut(int)
  | ShowValue(string, int)
  | ChangeText(string, int, int)
  | ClearOption(int, int)
  | ShowMore(int)
  | ShowItem(int)
  | CheckItem(int)
  | FilterItem(int)
  | DeleteItem(int)
  | AddItem(int)
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
  | AddForm(opticonitems) => {
      ...state,
      items:
        Array.append(
          state.items,
          newitem(
            state.items[Js_array.length(state.items) - 1].iid + 1,
            opticonitems,
          ),
        ),
    }
  | ClickBoardPaper(index) => {
      ...state,
      items:
        Array.mapi(
          (i, item) => {...item, showLine: index == i},
          state.items,
        ),
    }
  | ChangeTitle(value, index) => {
      ...state,
      items:
        Array.mapi(
          (i, item) =>
            index == i ? {...item, title: value, itemModify: true} : item,
          state.items,
        ),
    }
  | ShowOut(index) => {
      ...state,
      items:
        Array.mapi(
          (i, item) =>
            {...item, showOut: index == i ? !item.showOut : false},
          state.items,
        ),
    }
  | ShowValue(outValue, index) => {
      ...state,
      items:
        Array.mapi(
          (i, item) =>
            index == i
              ? {...item, outValue, showOut: false, itemModify: true} : item,
          state.items,
        ),
    }
  | ChangeText(value, rindex, index) => {
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
                      rindex == ri ? {...answeritem, value} : answeritem,
                    item.answeritems,
                  ),
                itemModify: true,
              }
              : item,
          state.items,
        ),
    }
  | ClearOption(rindex, index) => {
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
                        ? {...answeritem, ansrDelete: !answeritem.ansrDelete}
                        : answeritem,
                    item.answeritems,
                  ),
                itemModify: true,
              }
              : item,
          state.items,
        ),
    }
  | ShowMore(index) => {
      ...state,
      items:
        Array.mapi(
          (i, item) =>
            index == i ? {...item, showMore: !item.showMore} : item,
          state.items,
        ),
    }
  | ShowItem(index) => {
      ...state,
      items:
        Array.mapi(
          (i, item) =>
            index == i
              ? {...item, showShow: !item.showShow, itemModify: true} : item,
          state.items,
        ),
    }
  | CheckItem(index) => {
      ...state,
      items:
        Array.mapi(
          (i, item) =>
            index == i
              ? {
                ...item,
                showShow: !item.showCheck,
                showCheck: !item.showCheck,
                itemModify: true,
              }
              : item,
          state.items,
        ),
    }
  | FilterItem(index) => {
      ...state,
      items:
        Array.mapi(
          (i, item) =>
            index == i
              ? {...item, showFilter: !item.showFilter, itemModify: true}
              : item,
          state.items,
        ),
    }
  | DeleteItem(index) => {
      ...state,
      items:
        Array.mapi(
          (i, item) =>
            index == i
              ? {...item, itemDelete: !item.itemDelete, itemModify: true}
              : item,
          state.items,
        ),
    }
  | AddItem(index) => {
      ...state,
      items:
        Array.mapi(
          (i, item) =>
            index == i
              ? {
                ...item,
                answeritems:
                  Array.append(
                    item.answeritems,
                    newansweritem(
                      state.items[i].answeritems[Js_array.length(
                                                   item.answeritems,
                                                 )
                                                 - 1].
                        id
                      + 1,
                    ),
                  ),
                itemModify: true,
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

[@react.component]
let make = _ => {
  let (state, dispatch) = useReducer(reducer, initialState);

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
      |> Axiosapi.Formor.search
      |> then_(response =>
           {
             switch (response##data##status) {
             | "istrue" =>
               SettingFormItems(response##data##items) |> dispatch;
               ActionShowProgress |> dispatch;
             | _ =>
               SettingError |> dispatch;
               response##data##status
               |> Status.statusModule
               |> barShowRestoreAction;
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
      |> iItemsData(
           Js_array.filter(
             (item: item) => item.itemModify === true,
             state.items,
           ),
         )
      |> Axiosapi.Formor.insert
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

  let clickBoardPaper = useCallback(i => ClickBoardPaper(i) |> dispatch);

  let changeTitle =
    useCallback((value, i) => ChangeTitle(value, i) |> dispatch);

  let showOut = useCallback(i => ShowOut(i) |> dispatch);

  let showValue = useCallback((value, i) => ShowValue(value, i) |> dispatch);

  let changeText =
    useCallback((value, ri, i) => ChangeText(value, ri, i) |> dispatch);

  let clearOption = useCallback((ri, i) => ClearOption(ri, i) |> dispatch);

  let showMore = useCallback(i => ShowMore(i) |> dispatch);

  let showItem = useCallback(i => ShowItem(i) |> dispatch);

  let checkItem = useCallback(i => CheckItem(i) |> dispatch);

  let filterItem = useCallback(i => FilterItem(i) |> dispatch);

  let deleteItem = useCallback(i => DeleteItem(i) |> dispatch);

  let addItem = useCallback(i => AddItem(i) |> dispatch);

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
          {state.items
           |> Array.mapi((i, item) =>
                <GridItem top="0" right="24" left="24" xs="auto">
                  <CardOrPaperBoard
                    onClick={_ => i |> clickBoardPaper}
                    showLine={item.showLine}>
                    <GridContainer
                      direction="column" justify="start" alignItem="stretch">
                      <GridItem top="0" bottom="0" xs="auto">
                        <GridContainer
                          direction="row" justify="center" alignItem="center">
                          <GridItem
                            top="0" right="0" bottom="0" left="0" xs="auto">
                            <TextFieldStandard
                              top="0"
                              right="0"
                              left="0"
                              value={item.title}
                              disabled={state.showProgress || item.itemDelete}
                              onChange={event =>
                                i
                                |> changeTitle(
                                     ReactEvent.Form.target(event)##value,
                                   )
                              }>
                              null
                            </TextFieldStandard>
                          </GridItem>
                          {item.showLine
                             ? <>
                                 <GridItem
                                   top="0"
                                   right="0"
                                   bottom="0"
                                   left="0"
                                   xs="no">
                                   <IconButton
                                     padding="10"
                                     disabled={
                                       state.showProgress || item.itemDelete
                                     }>
                                     <IconAction
                                       animation="leftRight"
                                       src=collectionsBlack
                                     />
                                   </IconButton>
                                 </GridItem>
                                 <GridItem
                                   top="0"
                                   right="0"
                                   bottom="0"
                                   left="0"
                                   width="156px"
                                   xs="no">
                                   <SelectOutline
                                     top="0"
                                     borderColor="rgba(0,0,0,0.2)"
                                     value={item.outValue}
                                     disabled={
                                       state.showProgress || item.itemDelete
                                     }
                                     onClick={_ => i |> showOut}>
                                     ...(
                                          item.showOut && !item.itemDelete
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
                                                {item.opticonitems
                                                 |> Array.map(opticonitem =>
                                                      <MenuIcon
                                                        top="0"
                                                        right="8"
                                                        bottom="0"
                                                        left="8"
                                                        disablePadding={
                                                                    opticonitem.
                                                                    opticonPadding
                                                                    }
                                                        topLeft="12"
                                                        topRight="12"
                                                        bottomRight="12"
                                                        bottomLeft="12"
                                                        onClick={_ =>
                                                          i
                                                          |> showValue(
                                                               opticonitem.
                                                                 value,
                                                             )
                                                        }>
                                                        ...(
                                                             <IconGeneral
                                                               src={
                                                                    opticonitem.
                                                                    icon
                                                                   }
                                                             />,
                                                             opticonitem.value
                                                             |> string,
                                                           )
                                                      </MenuIcon>
                                                    )
                                                 |> array}
                                              </SelectMenu>
                                            : null,
                                          <IconGeneral
                                            animation={
                                              item.showOut |> topDownRorate
                                            }
                                            src=arrowDownBlack
                                          />,
                                        )
                                   </SelectOutline>
                                   <BackgroundBoard
                                     showBackground={item.showOut}
                                     backgroundColor="transparent"
                                     onClick={_ => i |> showOut}
                                   />
                                 </GridItem>
                               </>
                             : null}
                        </GridContainer>
                      </GridItem>
                      <GridItem top="0" bottom="0" xs="auto">
                        {switch (item.outValue) {
                         | "label" =>
                           <Typography
                             variant="subtitle2"
                             style={ReactDOMRe.Style.make(
                               ~paddingLeft="6px",
                               ~paddingRight="6px",
                               (),
                             )}
                             noWrap=true>
                             null
                           </Typography>
                         | "image" =>
                           <ImageUpload
                             webLoad={state.showProgress}
                             showDrop={item.showDrop}
                             showFile={item.showFile}
                             disabled=true
                           />
                         | "collections" =>
                           <ImageUpload
                             webLoad={state.showProgress}
                             showDrop={item.showDrop}
                             showFile={item.showFile}
                             disabled=true
                           />
                         | "text" =>
                           <TextFieldStandard
                             width="50"
                             top="0"
                             left="0"
                             enterBorderColor="rgba(255,0,0,0.8)"
                             downBorderColor="rgba(255,0,0,0.6)"
                             borderColor="rgba(0,0,0,0.2)"
                             disabled=true>
                             null
                           </TextFieldStandard>
                         | "textline" =>
                           <TextFieldStandard
                             top="0"
                             left="0"
                             enterBorderColor="rgba(255,0,0,0.8)"
                             downBorderColor="rgba(255,0,0,0.6)"
                             borderColor="rgba(0,0,0,0.2)"
                             disabled=true>
                             null
                           </TextFieldStandard>
                         | "textarea" =>
                           <TextFieldMultiline
                             top="12"
                             bottom="0"
                             left="0"
                             labelColor="rgba(255,0,0,0.8)"
                             borderTop="10"
                             borderBottom="10"
                             enterBorderColor="rgba(255,0,0,0.8)"
                             downBorderColor="rgba(255,0,0,0.6)"
                             borderColor="rgba(0,0,0,0.2)"
                             rows=3
                             disabled=true>
                             null
                           </TextFieldMultiline>
                         | "droplist" =>
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
                                           placeholder="Option"
                                           value={answeritem.value}
                                           disabled={
                                             state.showProgress
                                             || item.itemDelete
                                             || answeritem.ansrDelete
                                           }
                                           onChange={event =>
                                             i
                                             |> changeText(
                                                  ReactEvent.Form.target(
                                                    event,
                                                  )##value,
                                                  ai,
                                                )
                                           }>
                                           null
                                         </TextFieldStandard>
                                       </GridItem>
                                       {item.showLine
                                          ? <GridItem
                                              top="0"
                                              right="0"
                                              bottom="0"
                                              left="0"
                                              xs="no">
                                              <IconButton
                                                padding="4"
                                                disabled={
                                                  state.showProgress
                                                  || item.itemDelete
                                                }
                                                onClick={_ =>
                                                  i |> clearOption(ai)
                                                }>
                                                <IconAction
                                                  animation="circle"
                                                  src={
                                                    answeritem.ansrDelete
                                                      ? refreshBlack
                                                      : clearWarn
                                                  }
                                                />
                                              </IconButton>
                                            </GridItem>
                                          : null}
                                     </GridContainer>
                                   </GridItem>
                                 )
                              |> array}
                           </GridContainer>
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
                                               false
                                               |> answerIcon(item.outValue)
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
                                           placeholder="Option"
                                           value={answeritem.value}
                                           disabled={
                                             state.showProgress
                                             || item.itemDelete
                                           }
                                           onChange={event =>
                                             i
                                             |> changeText(
                                                  ReactEvent.Form.target(
                                                    event,
                                                  )##value,
                                                  ai,
                                                )
                                           }>
                                           null
                                         </TextFieldStandard>
                                       </GridItem>
                                       {item.showLine
                                          ? <GridItem
                                              top="0"
                                              right="0"
                                              bottom="0"
                                              left="0"
                                              xs="no">
                                              <IconButton
                                                padding="4"
                                                disabled={
                                                  state.showProgress
                                                  || item.itemDelete
                                                }
                                                onClick={_ =>
                                                  i |> clearOption(ai)
                                                }>
                                                <IconAction
                                                  animation="circle"
                                                  src={
                                                    answeritem.ansrDelete
                                                      ? refreshBlack
                                                      : clearWarn
                                                  }
                                                />
                                              </IconButton>
                                            </GridItem>
                                          : null}
                                     </GridContainer>
                                   </GridItem>
                                 )
                              |> array}
                           </GridContainer>
                         }}
                      </GridItem>
                      {item.showLine
                         ? <>
                             <GridItem xs="auto"> <Divider /> </GridItem>
                             <GridItem
                               top="0" right="0" bottom="0" left="0" xs="auto">
                               <GridContainer
                                 direction="rowReverse"
                                 justify="start"
                                 alignItem="center">
                                 <GridItem
                                   top="0"
                                   right="0"
                                   bottom="0"
                                   left="0"
                                   xs="no">
                                   <IconButton
                                     padding="8"
                                     disabled={
                                       state.showProgress || item.itemDelete
                                     }
                                     onClick={_ => i |> showMore}>
                                     <Tooltip
                                       location="top"
                                       backgroundColor="rgba(255,0,0,0.8)">
                                       <FormattedMessage
                                         id="more"
                                         defaultMessage="More"
                                       />
                                     </Tooltip>
                                     <IconAction
                                       animation="circle"
                                       src=moreVertBlack
                                     />
                                   </IconButton>
                                   {item.showMore && !item.itemDelete
                                      ? <SelectMenu
                                          top="100%"
                                          right="0"
                                          transform="translate(0, -100%)"
                                          maxWidth="256"
                                          width="256"
                                          maxHeight="280"
                                          minHeight="0"
                                          topLeft="12"
                                          topRight="12"
                                          bottomRight="12"
                                          bottomLeft="12"
                                          paddingRight="8"
                                          paddingLeft="8">
                                          {switch (item.outValue) {
                                           | "radio" =>
                                             <>
                                               <MenuIcon
                                                 top="0"
                                                 right="8"
                                                 bottom="0"
                                                 left="8"
                                                 disablePadding=true
                                                 topLeft="12"
                                                 topRight="12"
                                                 bottomRight="12"
                                                 bottomLeft="12">
                                                 ...(
                                                      <IconGeneral
                                                        src=doneSuccessful
                                                      />,
                                                      <FormattedMessage
                                                        id="desc"
                                                        defaultMessage="Desc"
                                                      />,
                                                    )
                                               </MenuIcon>
                                               <MenuIcon
                                                 top="0"
                                                 right="8"
                                                 bottom="0"
                                                 left="8"
                                                 disablePadding=true
                                                 topLeft="12"
                                                 topRight="12"
                                                 bottomRight="12"
                                                 bottomLeft="12">
                                                 ...(
                                                      <IconGeneral
                                                        src=doneSuccessful
                                                      />,
                                                      <FormattedMessage
                                                        id="Formor.relevant"
                                                        defaultMessage="Relevant"
                                                      />,
                                                    )
                                               </MenuIcon>
                                             </>
                                           | "checkbox" =>
                                             <MenuIcon
                                               top="0"
                                               right="8"
                                               bottom="0"
                                               left="8"
                                               disablePadding=true
                                               topLeft="12"
                                               topRight="12"
                                               bottomRight="12"
                                               bottomLeft="12">
                                               ...(
                                                    <IconGeneral
                                                      src=doneSuccessful
                                                    />,
                                                    <FormattedMessage
                                                      id="desc"
                                                      defaultMessage="Desc"
                                                    />,
                                                  )
                                             </MenuIcon>
                                           | _ =>
                                             <MenuIcon
                                               top="0"
                                               right="8"
                                               bottom="0"
                                               left="8"
                                               disablePadding=true
                                               topLeft="12"
                                               topRight="12"
                                               bottomRight="12"
                                               bottomLeft="12">
                                               ...(
                                                    <IconGeneral
                                                      src=doneSuccessful
                                                    />,
                                                    <FormattedMessage
                                                      id="desc"
                                                      defaultMessage="Desc"
                                                    />,
                                                  )
                                             </MenuIcon>
                                           }}
                                        </SelectMenu>
                                      : null}
                                   <BackgroundBoard
                                     showBackground={item.showMore}
                                     backgroundColor="transparent"
                                     onClick={_ => i |> showMore}
                                   />
                                 </GridItem>
                                 <GridItem
                                   top="0"
                                   right="0"
                                   bottom="0"
                                   left="6"
                                   xs="no">
                                   <Switch
                                     right="0"
                                     checked={item.showShow}
                                     circleColor={item.showShow |> circle}
                                     linearColor={item.showShow |> linear}
                                     fontColor={item.showShow |> font}
                                     disabled={
                                       state.showProgress || item.itemDelete
                                     }
                                     onClick={_ => i |> showItem}>
                                     <FormattedMessage
                                       id="Formor.show"
                                       defaultMessage="Show"
                                     />
                                   </Switch>
                                 </GridItem>
                                 <GridItem
                                   top="0"
                                   right="0"
                                   bottom="0"
                                   left="6"
                                   xs="no">
                                   <Switch
                                     right="0"
                                     checked={item.showCheck}
                                     circleColor={item.showCheck |> circle}
                                     linearColor={item.showCheck |> linear}
                                     fontColor={item.showCheck |> font}
                                     disabled={
                                       state.showProgress || item.itemDelete
                                     }
                                     onClick={_ => i |> checkItem}>
                                     <FormattedMessage
                                       id="need"
                                       defaultMessage="Need"
                                     />
                                   </Switch>
                                 </GridItem>
                                 <GridItem
                                   top="0"
                                   right="0"
                                   bottom="0"
                                   left="0"
                                   xs="no">
                                   <Switch
                                     right="0"
                                     checked={item.showFilter}
                                     circleColor={item.showFilter |> circle}
                                     linearColor={item.showFilter |> linear}
                                     fontColor={item.showFilter |> font}
                                     disabled={
                                       state.showProgress || item.itemDelete
                                     }
                                     onClick={_ => i |> filterItem}>
                                     <FormattedMessage
                                       id="Formor.filter"
                                       defaultMessage="Filter"
                                     />
                                   </Switch>
                                 </GridItem>
                                 <GridItem
                                   style={ReactDOMRe.Style.make(
                                     ~marginRight="12px",
                                     ~borderRight="1px solid rgba(0,0,0,0.12)",
                                     (),
                                   )}
                                   left="0"
                                   xs="no">
                                   null
                                 </GridItem>
                                 <GridItem
                                   top="0"
                                   right="0"
                                   bottom="0"
                                   left="0"
                                   xs="no">
                                   <IconButton
                                     padding="8"
                                     disabled={state.showProgress}
                                     onClick={_ => i |> deleteItem}>
                                     <Tooltip
                                       location="top"
                                       backgroundColor="rgba(255,0,0,0.8)">
                                       <FormattedMessage
                                         id={
                                           item.itemDelete
                                             ? "refresh" : "deleted"
                                         }
                                         defaultMessage={
                                           item.itemDelete
                                             ? "Refresh" : "Deleted"
                                         }
                                       />
                                     </Tooltip>
                                     <IconAction
                                       animation="leftRight"
                                       src={
                                         item.itemDelete
                                           ? refreshBlack : deleteBlack
                                       }
                                     />
                                   </IconButton>
                                 </GridItem>
                                 <GridItem
                                   top="0"
                                   right="0"
                                   bottom="0"
                                   left="0"
                                   xs="no">
                                   <IconButton
                                     padding="8"
                                     disabled={
                                       state.showProgress || item.itemDelete
                                     }>
                                     <Tooltip
                                       location="top"
                                       backgroundColor="rgba(255,0,0,0.8)">
                                       <FormattedMessage
                                         id="copy"
                                         defaultMessage="Copy"
                                       />
                                     </Tooltip>
                                     <IconAction
                                       animation="leftRight"
                                       src=fileCopyBlack
                                     />
                                   </IconButton>
                                 </GridItem>
                                 {switch (item.outValue) {
                                  | "radio"
                                  | "checkbox"
                                  | "droplist" =>
                                    <GridItem
                                      top="0"
                                      right="0"
                                      bottom="0"
                                      left="0"
                                      xs="no">
                                      <IconButton
                                        padding="8"
                                        disabled={
                                          state.showProgress || item.itemDelete
                                        }
                                        onClick={_ => i |> addItem}>
                                        <Tooltip
                                          location="top"
                                          backgroundColor="rgba(255,0,0,0.8)">
                                          <FormattedMessage
                                            id="add"
                                            defaultMessage="Add"
                                          />
                                        </Tooltip>
                                        <IconAction
                                          animation="circle"
                                          src=addBlack
                                        />
                                      </IconButton>
                                    </GridItem>
                                  | _ => null
                                  }}
                               </GridContainer>
                             </GridItem>
                           </>
                         : null}
                    </GridContainer>
                  </CardOrPaperBoard>
                </GridItem>
              )
           |> array}
        </GridContainer>
      </GridItem>
    </NewFacetube>
    <SnackbarYoutube showYoutube={state.showYoutube} position="bottomLeft">
      ...(<span> {state.youtubeText |> string} </span>, null)
    </SnackbarYoutube>
  </>;
};
