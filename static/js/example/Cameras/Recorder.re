open React;
let make: unit => element = [%bs.raw
  stream => {| return new MediaRecorder(stream, { nimeType: 'video/webm;codecs=VP9' }); |}
];
//[@bs.new] external make: element => Js.t('a) = "MediaRecorder";
/*let start: (element, int) => unit = [%bs.raw
  (mediaRecorder, time) => {| mediaRecorder.start(time); |}
];*/
[@bs.send] external start: element => unit = "start";
[@bs.send] external stop: element => unit = "stop";
