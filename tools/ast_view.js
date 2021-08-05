
// Flag to activate the display of all locations
var debug_locations = false;

// The imported JS file provides 'source' and 'contents' (TODO: 'language')
if (typeof source == 'undefined') {
  // TOOD: report error
}
var ast = contents;
let nodeid_root = 0;
let nodeid_invalid = -1;
var language_mode = language;
//---------------------------------------------------
// Code Mirror editor
// Documentation: https://codemirror.net/doc/manual.html

var editor;

// Initialize the editor
function initEditor() {
  editor = CodeMirror.fromTextArea(document.getElementById('source_code'), {
    mode: language, // or text/x-rustsrc TODO: use "language" from the exported file to select language
    lineNumbers: true,
    lineWrapping: true,
    readOnly: true,
    tabSize: 2,
    extraKeys: {
      'N': function(cm) { console.log("pressed N in editor"); },
    },
  });

  // Dimensions for codemirror window, if the code has more lines that the size of the window
  // a scrollbar will appear
  editor.setSize(700, 600);

  // Add a "getSelectedLoc" function
  // Returns a value in the form:
  // { start: { line: 1, col: 4 }, end: { line: 3, col: 8 } }
  editor.getSelectedLoc = function() {
    var from = editor.getCursor(true);
    var to = editor.getCursor(false);
    // Adding 1 because compilers counts from 1, and Codemirror from 0
    return { start: { line: from.line + 1, col: from.ch + 1 },
      end: { line: to.line + 1, col: to.ch + 1 } };
  };
}


//---------------------------------------------------
// Code Mirror highlight expressions

// Scroll to the given location in the given CodeMirror or Doc object.
function scrollToLoc(loc) {
  editor.scrollIntoView(loc, 100);
}

// Marks a region of source code as highlighted
// The "loc" argument should be of the form:
//  { start: { line: 1, col: 4 }, end: { line: 3, col: 8 } }
function addToSelection(loc) {
  // for other options, see https://codemirror.net/doc/manual.html#markText
  const opts = { className: "highlight" };
  if (loc === void(0) || loc === "") {
    return;
  }
  // Substracting 1 because compilers counts from 1, and Codemirror from 0
  var from = { line: loc.start.line-1, ch: loc.start.col-1 };
  var to = { line: loc.end.line-1, ch: loc.end.col-1 };

  // TODO: update generated locations to avoid empty span
  if (loc.start.line == loc.end.line && loc.start.col == loc.end.col) {
    loc.end.col++;
  }

  // Highlight and scroll to the highlighted place
  editor.markText(from, to, opts);
  scrollToLoc({from: from, to: to});
  // editor.focus();
}

// Clear existing highlighting
function resetSelection() {
  editor.getAllMarks().forEach(m => m.clear());
}

// Update the highlighted contents, by first removing previous selections
function updateSelection(loc) {
  resetSelection();
  addToSelection(loc);
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
// Handling click events in editor, to load the corresponding path in the AST

// We exploit the invariant that nodes from the AST are labelled in prefix order,
// meaning that deepest nodes have larger ids.

function number_of_nodeid(id) {
  return parseInt(id);
}

// This function checks whether a location is before another one (or equal)
function loc_before(pos1, pos2){
  return (pos1.line < pos2.line)
    || ((pos1.line == pos2.line)
       && (pos1.col <= pos2.col));
}

// This function returns true if the span of loc1 fully covers the the span of loc2
function contains(loc1, loc2){
  // Check if location is undefined; in this case don't consider this

  if (loc1 === "") {
    return false;
  } else {
    return loc_before(loc1.start, loc2.start)
        && loc_before(loc2.end, loc1.end);
  }
}

// Given a nodeid, build the path from the root to that node (inclusive),
// then load that path in the AST view.
function loadPathToNodeId(id) {
  let path_to_root = [];
  let cur_node = id;
  while (cur_node !== nodeid_invalid) {
    path_to_root.unshift(cur_node);
    cur_node = ast[cur_node].parent;
  }
  viewPath(path_to_root);
}

// This function is called when the user selects a span of code in the editor.
// It finds the corresponding node then loads it in the AST view.
function getNodeCorrespondingToSpan(selectedLoc) {
  if (debug_locations) {
    console.log(JSON.stringify(selectedLoc));
  }
  // find the deepest node in the AST that fully covers the location selected by the user
  let chosen_node = nodeid_root;
  for (const node_id in ast) {
    if (contains(ast[node_id].loc, selectedLoc)
      && (number_of_nodeid(node_id) >= number_of_nodeid(chosen_node))) {
      chosen_node = node_id;
    }
  }
}

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
var properties = [ "name", "parent", "type", "loc", "is_statement", "annot" ];

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

function viewDescription(id, node) { // node is ast[id]
  //console.log(node);
  if (! ("kind" in node)) {
    throw new Error("node without kind: " + id);
  }

  // The display depends on the kind
  var k = node.kind;

  // get all fields in the node description
  var keys = Object.keys(node);

  // remove special keys
  keys = keys.filter(item => item !== "kind" && item !== "children");

  // start by processing the generic properties
  var labelKind = html_span({'class': 'label-kind', onclick: "updateSelectedNode('" + id + "')"}, k);

  var txt = "";
  if (debug_locations) {
    txt += "[" + number_of_nodeid(id) + "] ";
  }

  txt += labelKind + " ";

  // start by processing the known specific properties
  for (iproperty in properties) {
    var key = properties[iproperty];
    if (key in node) {
      var value = node[key];
      // some keys have special display
      if (key == "name") {
        txt += "\"" + value + "\"" + "; ";
      } else if (key == "type") {
        txt += " : " + value;
      } else if (key == "parent") {
        if (debug_locations) {
          txt += "parent=" + value + "; ";
        }
      } else if (key == "loc") {
        if (debug_locations && value != "") {
          txt += " @ " + JSON.stringify(value);
          // value.start.line;
        }
      } else if (key == "is_statement") {
         // TODO: add this once we get the correct kind for each term

        if (value == false) {
          txt += "[expr] ";
        } else {
          txt += "[stat] ";
        }
        /**/

      // others use a generic display
      } else {
        // TODO: share code
        if (value !== "" && ! (Array.isArray(value) && value.length == 0)) {
          txt += " <i>"+key+"</i>: " + JSON.stringify(value);
        }
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
    if (value !== "" && ! (Array.isArray(value) && value.length == 0)) {
      txt += "; <i>"+key+"</i>: " + JSON.stringify(value);
    }
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
  // TODO: explain strategy
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
// Loading in AST and in editor

// Function to focus on a node in the AST and in the editor view
function loadViewToNodeId(id) {
  loadPathToNodeId(id);
  updateSelectedNode(id);
}


//---------------------------------------------------
// Handling events on the AST VIEW

function updateSelectedNode(id) {
  let node = ast[id];
  // Call updateSelection only in the case whne loc is non empty
  if (node.loc !== ""){
    updateSelection(node.loc);
  }
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
// Find decorated nodes

// returns an associative array from labels to nodeids
function getDecoratedNodes() {
  var results = [];
  for (var id in ast) {
    var node = ast[id];
    if (node.kind == "decoration") {
      results[node.label] = node.children[0].id;
    }
  }
  return results;
}

var decoratedNodes = getDecoratedNodes();

// displays the list of links to load decorated nodes, and highlight them
function initDecoratedNodes() {
  s = "";
  if (decoratedNodes.length > 0) {
    s += html_span({onclick: "selectAllDecoratedNodes()"}, "Highlighted:") + " ";
  }
  for (var label in decoratedNodes) {
    var id = decoratedNodes[label];
    s += html_span({onclick: "loadViewToNodeId(" + id + ")"}, "&nbsp;["+label+"]&nbsp;");
  }
  $("#targeted").html(s);
}

// display all the decorated nodes at once
function selectAllDecoratedNodes() {
  resetSelection();
  for (var label in decoratedNodes) {
    var id = decoratedNodes[label];
    var node = ast[id];
    addToSelection(node.loc);
    // if only one selection, load it into AST view
    if (decoratedNodes.length == 1) {
      loadPathToNodeId(id);
    }
  }
  if (decoratedNodes.length != 1) {
    loadPathToNodeId(nodeid_root);
  }
}


//---------------------------------------------------
// Main

// hook for codemirror
$(document).on('mouseup', '.CodeMirror', function () {
  var id = getNodeCorrespondingToSpan(editor.getSelectedLoc());
  loadViewToNodeId(id);
});

// action to perform after document is loaded
document.addEventListener('DOMContentLoaded', function () {
  // initialize parameter handlers
  initHandlers();

  // initialize editor with contents
  initEditor();
  editor.setValue(source);

  // show initial path: focused on the root
  var path = [nodeid_root];
  viewPath(path);

  // loads the decorated nodes, if any
  initDecoratedNodes();
  selectAllDecoratedNodes();
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

//---------------------------------------------------

/* TODO
   - load the ast and customize the display:
      - if value is empty, don't show the key
   - fix off by one column
   - check all locations match well in highlight
   - check user selection targets the right node each time
*/


// DEPRECATED, this function prints the selected location into the console
// $(document).on('mouseup', '.CodeMirror', function() {
//    console.log("Mouseup in code mirror");
//    console.log("Selected range: ");
//    console.log(editor.getSelectedLoc());
// });


// DEPRECATED NOW THE SCRIPT READS THE CURRENT SOURCE CODE DATA
// var exampleSource1 = `
//    /* C demo code */
//    #include <stdio.h>
//    int f() {
//       printf("hello f");
//       return 0;
//    }
//    int g() {
//       printf("hello g");
//       return 0;
//    }
//    int main() {
//       printf("hello world\n");
//       return 0;
//    }
// `;

// var node_1_loc = { start: { line: 7, col: 6 }, end: { line: 9, col: 15 } };


  // make some selection
  // var selection = { start: { line: 5, col: 6 }, end: { line: 6, col: 15 } };
  // updateSelection(selection);
