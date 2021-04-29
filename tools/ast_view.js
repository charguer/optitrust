

//---------------------------------------------------
// Code Mirror editor
// Documentation: https://codemirror.net/doc/manual.html

var editor = CodeMirror.fromTextArea(document.getElementById('source_code'), {
  mode: 'text/x-c++src',
  lineNumbers: true,
  lineWrapping: true,
  readOnly: true,
  tabSize: 2,
  extraKeys: {
    'N': function(cm) { console.log("pressed N in editor"); },
  },
});


//---------------------------------------------------
// Code Mirror highlight expressions

// Scroll to the given location in the given CodeMirror or Doc object.
function scrollToLoc(loc) {
  editor.scrollIntoView(loc, 100);
}

// Update the highlighted contents
// The "loc" argument should be of the form:
//  { start: { line: 1, col: 4 }, end: { line: 3, col: 8 } }

function updateSelection(loc) {
  // for other options, see https://codemirror.net/doc/manual.html#markText
  const opts = { className: "highlight" };
  if (loc === undefined) {
    return;
  }

  // Clear old marks
  editor.getAllMarks().forEach(m => m.clear());

  // Substracting 1 because compilers counts from 1, and Codemirror from 0
  var from = { line: loc.start.line-1, ch: loc.start.col };
  var to = { line: loc.end.line-1, ch: loc.end.col };

  // Highlight and scroll to the highlighted place
  editor.markText(from, to, opts);
  scrollToLoc({from: from, to: to});
  // editor.focus();
}


//---------------------------------------------------
// Handling parameters

var parameter_depth = 0; // 0 means infinity

// Event listner for the depth parameter
$("#parameter_depth").change(function(e) {
  // get the value as an integer
  var n = + $("#parameter_depth").val();
  parameter_depth = n;
  console.log("Changed parameter depth to: " + n);
});

// Event listner for the run button
$("#button_run").click(function (e) {
   console.log("Click on run button");
});


//---------------------------------------------------
// DEMO

var exampleSource = `
   /* C demo code */
   #include <stdio.h>
   int f() {
      printf("hello f");
      return 0;
   }
   int g() {
      printf("hello g");
      return 0;
   }
   int main() {
      printf("hello world\n");
      return 0;
   }
`;

editor.setValue(exampleSource);

var selection = { start: { line: 5, col: 6 }, end: { line: 6, col: 15 } };

updateSelection(selection);



//---------------------------------------------------
// FOR LATER: function to scroll between marks

/*
// Scroll to the first mark in the given CodeMirror or Doc object.
// No-op if given object not active, or if no marks in the doc.
function scrollToFirstMark() {
  let ms = doc.getAllMarks();
  if (ms.length < 1) return;
  let loc = ms[0].find();
  scrollToLoc(loc);
}
*/

