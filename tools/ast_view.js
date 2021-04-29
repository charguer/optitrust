/*
//---------------------------------------------------
// For debugging

// Demo code if no code provided
if (codeString == undefined) {
   codeString = 'var t = [];\nfor (var i = 0; i < 3; i++) {\n  t[i] = (function(j) {\n      return function() { return j; }; \n    })(i); \n};\nt[0](); ';
}

//---------------------------------------------------
// CodeMirror tooling

// Core Mirror objects
var source = CodeMirror.fromTextArea(document.querySelector('#source_code'), {
  mode: 'text/x-c++src',
  lineNumbers: true,
  lineWrapping: true,
  readOnly: true,
  tabSize: 2,
  value: codeString
  extraKeys: {
    'N': function(cm) { next(); },
  },
});

// Registers a new source doc
function newSourceDoc(name, text, readOnly) {
  let doc = CodeMirror.Doc(text, 'text/javascript');
  doc.setName(name);
  doc.cantEdit = !!readOnly;
  return doc;
}

function updateSelectionInCodeMirror(loc, opts) {
  if (loc === undefined) {
    return;
  }

  // Clear old marks
  source.getAllMarks().forEach(m => m.clear());

  // Substracting 1 because Clang counts from 1, and Codemirror from 0
  var from = {line: loc.start.line-1, ch: loc.start.column };
  var to = {line: loc.end.line-1, ch: loc.end.column };

  source.markText(from, to, opts);
  scrollToLoc(source, {from: from, to: to});
}

// Scroll to the given location in the given CodeMirror or Doc object.
// No-op if the given doc is not currently active.
function scrollToLoc(loc) {
  if (source instanceof CodeMirror.Doc) {
    source = source.getEditor();
  }
  if (source) {
    source.scrollIntoView(loc, 100);
  }
}

// Scroll to the first mark in the given CodeMirror or Doc object.
// No-op if given object not active, or if no marks in the doc.
function scrollToFirstMark() {
  let ms = doc.getAllMarks();
  if (ms.length < 1) return;
  let loc = ms[0].find();
  scrollToLoc(loc);
}


//---------------------------------------------------
// Page behavior

function updateSelection(selection) {
  const selectionOpts = { className: "highlight" };
  var from = {line: loc.start.line-1, ch: loc.start.column };
  var to = {line: loc.end.line-1, ch: loc.end.column };
  updateSelectionInCodeMirror(selection, selectionOpts);
  interpreter.focus();
}

var selection = { start: { line: 2, col: 3 }, end: { line: 4, col: 6 } };
updateSelection(selection);

function next() {
   selection.end.line = selection.end.line+1;
   updateSelection(selection);
   $("#viewast").html("foo " + selection.end.line);
}


var current_id = null;
var parameter_depth = 0; // 0 means infinity

// Event listner for the depth parameter
$("#parameter_depth").change(function(e) {
  var n = + $("#parameter_depth").val();
  parameter_depth = n;
});


*/