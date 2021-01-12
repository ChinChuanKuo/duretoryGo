open React;
open Together;
open Icons;
open Data;
open Axiosapi;
open Status;
open Storage;
open ObjectFormat;
[%bs.raw {|require('../../../scss/pages/Together/together.scss')|}];

type collitem = {
  subId: string,
  value: string,
};

type item = {
  id: string,
  collitems: array(collitem),
  attribute: string,
  creator: string,
  datetime: string,
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
  showYoutube: bool,
  youtubeText: string,
};

type action =
  | SettingError
  | SettingFormLoad
  | SettingFormWidth(int, int)
  | ActionShowProgress
  | ActionPermissItems(bool, bool, bool, bool)
  | SettingFormItems(bool, int, array(item))
  | SettingScrollItems(bool, array(item))
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
      "newid"
      |> Locals.select
      |> otherData(length)
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

  <>
    <NewFacetube showProgress={state.showProgress} error={state.error}>
      <GridItem
        style=marginAuto top="0" right="32" bottom="0" left="32" xs="12">
        <GridContainer direction="column" justify="center" alignItem="stretch">
          <GridItem right="24" bottom="0" left="24" xs="auto">
            <GridContainer direction="row" justify="start" alignItem="center">
              {state.items
               |> Array.mapi((i, item) =>
                    <div>
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
                              top="0" right="0" bottom="0" left="0" xs="no">
                              <div
                                style={ReactDOMRe.Style.make(
                                  ~height="155px",
                                  (),
                                )}
                              />
                            </GridItem>
                            <GridItem
                              top="0"
                              right="12"
                              bottom="0"
                              left="16"
                              xs="auto">
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
                                  <Typography variant="subheading" noWrap=true>
                                    {item.attribute |> string}
                                  </Typography>
                                </GridItem>
                              </GridContainer>
                            </GridItem>
                            <GridItem
                              top="0"
                              right="12"
                              bottom="0"
                              left="72"
                              xs="auto">
                              <Typography variant="caption" color="#606060">
                                {item.datetime |> string}
                              </Typography>
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
      </GridItem>
    </NewFacetube>
    <SnackbarYoutube showYoutube={state.showYoutube} position="bottomLeft">
      ...(<span> {state.youtubeText |> string} </span>, null)
    </SnackbarYoutube>
  </>;
};
