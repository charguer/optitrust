

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
// Auxiliary functions for manipulating html dynamically

// WARNING: don't use double quotes in "args" (they need to be escaped)!
function html_element(kind, args, contents) {
  var sargs = "";
  for (var arg in args) {
    sargs += ' ' + arg + '="' + args[arg] + '" ';
  }
  return '<' + kind + sargs + '>' + contents + '</' + kind + '>';
}

function html_div(args, contents) {
  return html_element('div', args, contents);
}

function html_span(args, contents) {
  return html_element('span', args, contents);
}


//---------------------------------------------------
// loading of a node in the AST view

var ast;

function get_child_label(node, id_child) {
  if (! "children" in node) {
    return null;
  }
  var nb = node.children.length;
  for (var i = 0; i < nb; i++) {
    if (node.children[i].id == id_child) {
      return node.children[i].label;
    }
  }
  return null;
}


// auxiliary function for viewPath,
// path should be a list of node ids
// target should be the name of a div
function viewPathRec(path, target, label) {
  console.log("viewing in " + target + " : "   + path);
  // get first node in path, and compute remaining path
  var id = path.shift();
  var node = ast[id];

  // build description
  var k = node.kind;
  var txt = "<b>" + k + "</b>";
  if (k == "var" || k == "fun") {
    txt += " " + node.name;
  }
  // build buttons
  var ctrl = html_span({'onclick': "console.log('plus')"}, "&CirclePlus;") +
             html_span({'onclick': "console.log('minus')"}, "&CircleMinus;");

  // build html
  var descr = html_span({class: "ast_label"}, label) +
              html_span({class: "ast_ctrl"}, ctrl) +
              html_span({class: "ast_txt"}, txt);

  // build div for that node
  var div_view = id + "_view";
  var div_descr = id + "_descr";
  var div_children = id + "_children";
  $("#"+target).append(html_div({ id: div_view, class: "ast_node" }, ""));
  $("#"+div_view).append(html_div({ id: div_descr, class: "ast_descr" }, descr));
  $("#"+div_view).append(html_div({ id: div_children, class: "ast_children" }, ""));

  // if the path is not empty
  if (path.length > 0) {
    var id_child = path[0];
    // check that the next node is one of the children ids, and gets its label
    var label_child = get_child_label(node, id_child);
    if (label_child === null) {
      console.log("invalid path: " + id + " does not have a child " + id_child);
      console.log(node);
      return;
    }
    // continue with what remains of the path
    viewPathRec(path, div_children, label_child);
  }
}



// Loads the view of a path
// path should be a list of node ids
function viewPath(path) {
   viewPathRec(path, "viewast", "root");
}

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

ast = {
   node_0: { kind: "seq", children: [ { label: "1", id: "node_1" }, { label: "2", id: "node_2" } ] },
   node_1: { kind: "fun", name: "foo", children: [ { label: "body", id: "node_3" } ] },
   node_2: { kind: "var", name: "x" },
   node_3: { kind: "return" } };

// action to perform after document is loaded
document.addEventListener('DOMContentLoaded', function () {
  // reset the contents
  $("#viewast").html("");
  // show demo path
  viewPath(["node_0", "node_1", "node_3" ]);

});

//viewPath(["node_3"]);

   // TODO: if children is empty, no need to include this field in the JSON.


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

