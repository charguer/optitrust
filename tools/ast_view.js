

//---------------------------------------------------
// Code Mirror editor
// Documentation: https://codemirror.net/doc/manual.html

var editor;

// Initialize the editor
function initEditor() {
  editor = CodeMirror.fromTextArea(document.getElementById('source_code'), {
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
}


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

function initHandlers() {

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

}

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

// order in which to display properties
// properties that are not mentioned are processed at the end in arbitrary order
var properties = [ "name", "type" ];


var ast;

function get_child_label(node, id_child) {
  if (! ("children" in node)) {
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

function viewDescription(id, node) {
  //console.log(node);
  var k = node.kind;
  
  // get all fields in the node description
  var keys = Object.keys(node);

  // remove special keys
  keys = keys.filter(item => item !== "kind" && item !== "children");

  // start by processing the generic properties
  var labelKind = html_span({'class': 'label-kind', onclick: "updateSelectedNode('" + id + "')"}, k);
  
  var txt = labelKind + " ";

  // start by processing the known specific properties
  for (iproperty in properties) {
    var key = properties[iproperty];
    if (key in node) {
      var value = node[key];
      // some keys have special display
      if (key == "name") {
        txt += value;
      } else if (key == "type") {
        txt += " : " + value;
      // others use a generic display
      } else {
        txt += " <i>"+key+"</i>: " + value;
      }

    }
    // remove the key when processed
    keys = keys.filter(item => item !== key);
  }

  // then process unknown properties
  for (ikey in keys) {
    var key = keys[ikey];
    var value = node[key];
    // use a generic display (again)
    txt += "; <i>"+key+"</i>: " + value;
  }
  return txt;
}


// auxiliary function for viewPath,
// path should be a list of node ids
// target should be the name of a div
function viewPathRec(path, target, label, classExtra) {
  //console.log("viewing in " + target + " : "   + path);

  // get first node in path, and compute remaining path
  if (path.length == 0) { 
    throw new Error("viewPathRec: empty path");
  }
  var id = path.shift();
  var node = ast[id];

  // build description
  var txt = viewDescription(id, node);

  // build buttons, gray them if no valid operation
  var idchild = (path.length > 0) ? path[0] : "";
  var ctrlPlus = html_span({id: (id+"_plus"), onclick: "nodePlus('" + id + "', '" + idchild + "')"}, "&CirclePlus;");
  var ctrlMinus = html_span({id: (id+"_minus"), 'class': 'grayed', onclick: "nodeMinus('" + id + "')"}, "&CircleMinus;");
  var ctrl = ctrlPlus + ctrlMinus;
  var ctrlClass = "ast_ctrl";
  if ((! ("children" in node) || node.children.length == 0) // no children 
     || (node.children.length == 1 && path.length > 0)) { // one child and we explore it in the path
    ctrlClass += " grayed";
  }

  // build html
  var descrLabel = html_span({class: "ast_label"}, label);
  var descrCtrl = html_span({class: ctrlClass}, ctrl);
  var descrTxt = html_span({class: "ast_txt"}, txt);
  var descr = descrLabel + descrCtrl + " " + descrTxt;

  // build div for that node
  var div_view = id + "_view";
  var div_descr = id + "_descr";
  var div_children = id + "_children";

  var classNode = "ast_node";
  if (classExtra != undefined) {
    classNode += " " + classExtra;
  }
  $("#"+target).append(html_div({ id: div_view, class: classNode }, ""));
  $("#"+div_view).append(html_div({ id: div_descr, class: "ast_descr" }, descr));
  $("#"+div_view).append(html_div({ id: div_children, class: "ast_children" }, ""));

  // if the path is not empty
  if (path.length > 0) {
    var id_child = path[0];
    // check that the next node is one of the children ids, and gets its label
    var label_child = get_child_label(node, id_child);
    if (label_child === null) {
      console.log(node);
      throw new Error("invalid path: " + id + " does not have a child " + id_child);
    }
    // continue with what remains of the path
    viewPathRec(path, div_children, label_child);
  }
}

// Loads the view of a path
// path should be a list of node ids
function viewPath(path) {
   $("#viewast").html("");
   viewPathRec(path, "viewast", "root", "");
}


//---------------------------------------------------
// Handling events on the AST VIEW

function updateSelectedNode(id) {
  let node = ast[id];
  updateSelection(node.loc);
}

// Function to display all children of a given node,
// mark all but idchildKept using class 'ast_expanded'
// to allow the function nodeMinus to be simple to implement
function nodePlus(id, idchildKept) {
  var node = ast[id];

  // if no children, do nothing -- this should not happen because button is gray
  if (! ("children" in node) || node.children.length == 0) {
    return;
  }

  // save the one children currently expanded, if there is one
  var keptChild = null;
  var div_children = id + "_children";
  var children = $("#"+div_children).children();
  if (children.length == 1) {
    keptChild = children[0];
  }

  // clear the children
  var element = $("#"+div_children);
  element.empty();

  // populate all children
  for (var ichild in node.children) {
    var child = node.children[ichild];
    var idchild = child.id;
    if (idchild == idchildKept) {
      element.append(keptChild);
    } else {
      var label_child = get_child_label(node, idchild);
      viewPathRec([idchild], div_children, label_child, 'ast_expanded');
    }
  }

  // change gray buttons
  $("#"+id+"_plus").addClass("grayed");
  $("#"+id+"_minus").removeClass("grayed");
}

// Function to revert to the view of a single child of a given node.
function nodeMinus(id) {
  var div_children = id + "_children";
  $("#"+div_children).find(".ast_expanded").remove();
  // change gray buttons
  $("#"+id+"_plus").removeClass("grayed");
  $("#"+id+"_minus").addClass("grayed");
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

var node_1_loc = { start: { line: 7, col: 6 }, end: { line: 9, col: 15 } };

ast = {
   node_0: { kind: "seq", children: [ { label: "1", id: "node_1" }, { label: "2", id: "node_2" } , { label: "3", id: "node_4" }, { label: "4", id: "node_8" } ] },
   node_1: { kind: "fun", name: "foo", loc: node_1_loc, children: [ { label: "body", id: "node_3" } ] },
   node_2: { kind: "var", name: "x", type: "int" },
   node_3: { kind: "return" },
   node_4: { kind: "if", children: [ { label: "cond", id: "node_5" }, { label: "else", id: "node_6" }, { label: "else", id: "node_6" } ] },
   node_5: { kind: "return" },
   node_6: { kind: "return" },
   node_7: { kind: "return" },
   node_8: { kind: "return" } };
path = ["node_0", "node_1", "node_3" ];

if (typeof contents !== 'undefined') {
  ast = contents;
  path = ["node_0"];
}

// action to perform after document is loaded
document.addEventListener('DOMContentLoaded', function () {
  // initialize parameter handlers
  initHandlers();

  // initialize editor with contents
  initEditor();
  editor.setValue(exampleSource);

  // make some selection
  var selection = { start: { line: 5, col: 6 }, end: { line: 6, col: 15 } };
  updateSelection(selection);

  // show demo path
  viewPath(path);

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
  let loc = ms[0].find()
  editor.scrollIntoView(loc, 100);

}
*/


/* TODO
   - generate cpp code from ocaml (use base64 encoding?) (or assume no backtick or backslash in cpp code )
    var source_code = `
      ... (* put the contents like out_prog function is doing *)  
    `;
   - load cpp code in the html page
   - load the ast and customize the display
   - when user selects a range, construct the "path" and call viewPath on it
        -- get loc from the event
        -- first iterate over "ast" and find the deepest node (the one with biggest number) (+ (id.substr(5)))
           such that node.loc is covering the user selection (comparion function for {line: , col: })
        -- when node is found, you walk up the .parent fields, all the way to the root (until parent is -1 or no parent)
        -- at you walk up, fill an array "path" with the id  (use unshift, in your while loop)
   - when user clicks on a "kind" label, highlight the location in the code
*/

