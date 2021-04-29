

//---------------------------------------------------
// Code Mirror editor
// Documentation: https://codemirror.net/doc/manual.html

// Initialize the editor
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

// Add a "getSelectedLoc" function
// Returns a value in the form:
// { start: { line: 1, col: 4 }, end: { line: 3, col: 8 } }
editor.getSelectedLoc = function() {
  var from = editor.getCursor(true);
  var to = editor.getCursor(false);
  // Adding 1 because compilers counts from 1, and Codemirror from 0
  return { start: { line: from.line + 1, col: from.ch },
    end: { line: to.line + 1, col: to.ch } };
};


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

// FOR FUTURE USE

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
// Handling selection and click events in editor

$(document).on('mouseup', '.CodeMirror', function() {
   console.log("Mouseup in code mirror");
   console.log("Selected range: ");
   console.log(editor.getSelectedLoc());
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
// TODO

/*

NOW:

- Initially, load in the AST view the root node,

- On a mouse up event, find the deepest node in the AST whose range
fully covers the selection (bigger node ids means deeper node).
Construct the list of nodes from the AST root to that node.
Invoke the function that displays the view for those nodes.

- On a click on a node in the AST view, highlight the corresponding
piece of code in the editor.



-----
LATER:

Above code mirror, display a div with the list of versions,
each version number corresponds to the line from the ml file
that the version comes from.

Click on a version number should load the corresponding code
and clear the AST node view.

Place a "diff" button next to it. Click on a diff button, then
on a version number prints the diff between two versions.


*/



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

