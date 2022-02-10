

//---------------------------------------------------
// Code Mirror editor
// Documentation: https://codemirror.net/doc/manual.html

var editor;

// Initialize the editor
function initEditor() {
  editor = CodeMirror.fromTextArea(document.getElementById('source_code'), {
    mode: "text/x-c++src", // or text/x-rustsrc
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

  // CURRENTLY NOT USED
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

function scrollToLoc(loc) { // ({from: from, to: to});
  editor.scrollIntoView(loc, 100);
}

// hook for codemirror
/*
$(document).on('mouseup', '.CodeMirror', function () {
  // triggered on mouseup events in codemirror, eg to update selection
  // editor.getSelectedLoc()
});*/


//---------------------------------------------------
// Diff2html

var configuration = {
   inputFormat: 'json',
   drawFileList: false,
   // fileListToggle: false,
   // fileListStartVisible: false
   fileContentToggle: false,
   matching: 'lines',
   outputFormat: 'side-by-side',
   synchronisedScroll: true,
   highlight: true,
   renderNothingWhenEmpty: false,
   // LATER tune?
   // matchWordsThreshold : similarity threshold for word matching, default is 0.25
   // matchWordsThresholdmatchingMaxComparisons: perform at most this much comparisons for line matching a block of changes, default is 2500
   // maxLineSizeInBlockForComparison: maximum number os characters of the bigger line in a block to apply comparison, default is 200
   // maxLineLengthHighlight: only perform diff changes highlight if lines are smaller than this, default is 10000
   };

// TODO: the horizontal scrollbars should not be needed if the diff contains no long lines.


function loadDiffFromString(diffString) {
   // this function should be called only after DOM contents is loaded
  var targetElement = document.getElementById("diffDiv");
  var diff2htmlUi = new Diff2HtmlUI(targetElement, diffString, configuration);
  diff2htmlUi.draw();
  diff2htmlUi.highlightCode();
}

//---------------------------------------------------

// Could use Underscore's _escape method.
function escapeHTML(s) {
  return s.replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;');
}

function htmlButton(id, label, css, onclick) {
  return "<button id='" + id + "' class='" + css + "' type ='button' onclick='" + onclick + "'>" + label + "</button>";
}

var curSource = -1;
var curSdiff = -1;
var curBdiff = -1;

function resetView() {
  $("#sourceDiv").hide();
  $("#diffDiv").hide();
  curSource = -1;
  curSdiff = -1;
  curBdiff = -1;
  $(".ctrl-button").removeClass("ctrl-button-selected");
}

function loadSource(id) {
  resetView();
  $("#sourceDiv").show();
  editor.setValue(codes[id]);
  $("#button_code_" + id).addClass("ctrl-button-selected");
  curSource = id;
}

function loadSdiff(id) {
  resetView();
  $("#diffDiv").show();
  loadDiffFromString(smallsteps[id].diff);
  $("#button_sdiff_" + id).addClass("ctrl-button-selected");
  curSdiff = id;
}

function loadBdiff(id) {
  resetView();
  $("#diffDiv").show();
  loadDiffFromString(bigsteps[id].diff);
  $("#button_bdiff_" + id).addClass("ctrl-button-selected");
  curBdiff = id;
  curSdiff = bigsteps[id].start;
}

function nextSource() {
  if (curSource == -1 && curSdiff != -1) {
    curSource = curSdiff;
  }
  var id = Math.min(curSource + 1, codes.length-1);
  loadSource(id);
}

function nextSdiff() {
  if (curSdiff == -1 && curSource != -1) {
    curSdiff = curSource - 1;
  }
  var id = Math.min(curSdiff + 1, smallsteps.length-1);
  loadSdiff(id);
}

function nextBdiff() {
  if (curBdiff == -1) {
    if (curSdiff == -1 && curSource != -1) {
      curSdiff = curSource - 1;
    }
    /* compute the id of the first bigstep that contains curSdiff */
    for (var i = 0; i < bigsteps.length; i++) {
      if (bigsteps[i].start >= curSdiff) {
        curBdiff = i;
        break;
      }
    }
    curBdiff--; // anticipate for the +1 operation
  }
  var id = Math.min(curBdiff + 1, bigsteps.length-1);
  loadBdiff(id);
}

function initControls() {
  var s = "";
  function addRow(sTitle, sRow) {
    s += "<span class='row-title'>" + sTitle + ":</span>" + sRow + "<br/>";
  };

  // Code buttons
  var sCode = "";
  sCode += htmlButton("button_code_next", "next", "next-button", "nextSource()");
  for (var i = 0; i < codes.length; i++) {
    sCode += htmlButton("button_code_" + i, i, "ctrl-button", "loadSource(" + i + ")");
  }
  addRow("Source", sCode);

  // Small diff buttons
  var sSdiff = "";
  sSdiff += htmlButton("button_sdiff_next", "next", "next-button", "nextSdiff()");
  for (var i = 0; i < smallsteps.length; i++) {
    sSdiff += htmlButton("button_sdiff_" + i, (i+1), "ctrl-button", "loadSdiff(" + i + ")");
  }
  addRow("Diff", sSdiff);

  // Big diff buttons
  var sBdiff = "";
  sBdiff += htmlButton("button_bdiff_next", "next", "next-button", "nextBdiff()");
  for (var i = 0; i < bigsteps.length; i++) {
    sBdiff += htmlButton("button_bdiff_" + i, (i+1), "ctrl-button", "loadBdiff(" + i + ")");
  }
  addRow("BigDiff", sBdiff);

  $("#contents").html(s);
}

document.addEventListener('DOMContentLoaded', function () {
  initEditor();
  initControls();
  // editor.setValue("click on a button");
  // loadSource(codes.length-1);
  loadSource(0);

});
