open Icons;

[@genType]
let topDownRorate = showOut => showOut ? "justTopDown" : "";

[@genType]
let answerIcon = (outValue, showAnswer) =>
  switch (outValue) {
  | "checkbox" => showAnswer ? checkBoxBlack : checkBoxOutlineBlankBlack
  | _ => showAnswer ? radioButtonCheckedBlack : radioButtonUncheckedBlack
  };
