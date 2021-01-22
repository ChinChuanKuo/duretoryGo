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
  formId: string,
  showItem: bool,
  itemCount: int,
  items: array(item),
  showYoutube: bool,
  youtubeText: string,
};

type action =
  | SettingError
  | SettingFormLoad(string)
  | SettingFormWidth(int, int)
  | ActionShowProgress
  | ActionPermissItems(bool, bool, bool, bool)
  | SettingFormItems(bool, int, array(item))
  | SettingScrollItems(bool, array(item))
  | ShowCollections(int, int)
  | ClearForm(string)
  | ActionSnackBar(string, bool);

let reducer = (state, action) =>
  switch (action) {
  | SettingError => {...state, error: !state.error}
  | SettingFormLoad(value) => {
      ...state,
      formId: value,
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
  | ShowCollections(collectionIndex, index) => {
      ...state,
      items:
        Array.mapi(
          (i, item) => index == i ? {...item, index: collectionIndex} : item,
          state.items,
        ),
    }
  | ClearForm(id) => {
      ...state,
      itemCount: state.itemCount - 1,
      error: state.itemCount == 1,
      items: Js_array.filter((item: item) => item.id !== id, state.items),
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
  formId: "",
  showItem: false,
  itemCount: 0,
  items: [||],
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

let displayStyle = showDisplay => showDisplay ? "block" : "none";

[@react.component]
let make = _ => {
  let (state, dispatch) = useReducer(reducer, initialState);

  let barShowRestoreAction = youtubeText => {
    ActionSnackBar(youtubeText, true) |> dispatch;
    Js.Global.setTimeout(() => ActionSnackBar("", false) |> dispatch, 5000)
    |> ignore;
  };

  let searchAJax = formId =>
    Js.Promise.(
      "newid"
      |> Locals.select
      |> sRowsData(formId, state.items |> Js_array.length |> string_of_int)
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
             ReasonReactRouter.dangerouslyGetInitialUrl().hash |> searchAJax;
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

  let showPreviousCollections =
    useCallback((id, index, event) => {
      ReactEvent.Mouse.preventDefault(event);
      ReactEvent.Mouse.stopPropagation(event);
      let length = Js_array.length(state.items[index].collections) - 1;
      let collectionIndex = id == 0 ? length : id - 1;
      ShowCollections(collectionIndex, index) |> dispatch;
    });

  let showNextCollections =
    useCallback((id, index, event) => {
      ReactEvent.Mouse.preventDefault(event);
      ReactEvent.Mouse.stopPropagation(event);
      let length = Js_array.length(state.items[index].collections) - 1;
      let collectionIndex = id == length ? 0 : id + 1;
      ShowCollections(collectionIndex, index) |> dispatch;
    });

  let clickFormBoard = useCallback(id => ActionShowProgress |> dispatch);

  let editForm =
    useCallback((i, id, event) => {
      ReactEvent.Mouse.preventDefault(event);
      ReactEvent.Mouse.stopPropagation(event);
      ActionShowProgress |> dispatch;
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

  <>
    <NewFacetube showProgress={state.showProgress} error={state.error}>
      <GridItem
        style=marginAuto top="0" right="32" bottom="0" left="32" xs="12">
        <GridContainer direction="column" justify="center" alignItem="stretch">
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
                                        <div
                                          style={ReactDOMRe.Style.make(
                                            ~position="absolute",
                                            ~height="155px",
                                            ~left="50%",
                                            ~transform="translate(-50%, 0)",
                                            ~display=
                                              {item.index == ci |> displayStyle},
                                            (),
                                          )}>
                                          <Image
                                            width="auto"
                                            height="100%"
                                            borderRadius="6"
                                            src={
                                              "data:image/jpg;base64,"
                                              ++ collitem
                                            }
                                          />
                                        </div>
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
  </>;
};
