// The function [Trace.dump_trace_to_js] in [trace.ml] documents the data representation
// used for the trace. It looks like this, where 0 is always the id of the root step.

// TODO: restore the feature of displaying sources at lines

/*
var startupOpenStep = 45; // optional binding
var steps = [];
steps[0] = {
   id: 0, // corresponds to the step number
   kind: "..",
   exectime: 0.0453;   // in seconds
   name: "..",
   args: [ { name: "..", value: ".."}, { name: "..", value: ".." } ],
   tags: ["..", ".."],
   debug_msgs: ["..", ".."],
   justif: ["..", ".." ],
   check_validity: true,
   isvalid: true,
   script: window.atob("..."),
   script_line: 23, // possibly undefined
   astBefore: window.atob("..."), // NOT YET IMPLEMENTED; could also an id of an source code stored in a different array, for improved factorization
   astAfter: window.atob("..."), // NOT YET IMPLEMENTED
   diff: window.atob("..."), // could be slow if requested for all!
   sub: [ j1, j2, ... jK ]  // ids of the sub-steps
   }
step[1] = ...

Additional fields are set by the function   initTree
- parent_id
- has_valid_parent

The function initSteps() builds an array of [bigsteps] and an array of [smallsteps],
in order of appearance.

Representation of a big step:
  bigsteps[k] = step object with additional fields:
      bigstep_id: // the id of the step in the bigsteps array
      start: // an index in smallstep array, inclusive
      stop: // an index in smallstep array, exclusive
  smallsteps[k] = step object with additional fields:
      smallspep_id: // the id of the step in the smallsteps array

*/

// data structures filled by function initSteps
var smallsteps = [];
var bigsteps = [];
var hasBigsteps = undefined; // false iff bigsteps is empty


// checkbox status; may change default values here

var optionsDefaultValueForTags = { // true = checked = hidden
    "trivial": false,
    "valid_by_composition": false,
    "should_be_valid_by_composition": false,
    "simpl.arith": true,
    "IO": true,
    "target": true,
    "marks": true,
    "simpl": true,
    "typing": true,
   };

var allTags = {}; // filled by initAllTags

var optionsDescr = [ // extended by initAllTags
  { key: "tree",
    name: "tree",
    kind: "UI",
    default: true,
  },
  { key: "hide_substeps",
    name: "hide-substeps",
    kind: "UI",
    default: false,
  },
  { key: "ast_before",
    name: "ast-before",
    kind: "UI",
    default: false,
  },
  { key: "ast_after",
    name: "ast-after",
    kind: "UI",
    default: false,
  },
/*  { key: "stats",
    name: "stats",
    kind: "UI",
    default: false,
  }, */
  { key: "compact",
    name: "compact",
    kind: "UI",
    default: true,
  },
  { key: "step_change",
    name: "step-change",
    kind: "UI",
    default: false,
  },
  { key: "args",
    name: "arguments",
    kind: "UI",
    default: true,
  },
  { key: "justif",
    name: "justification",
    kind: "UI",
    default: false,
  },
  /* always-true
  { key: "simpl-show",
    name: "simpl-show-steps",
    kind: "UI",
    default: true,
  },*/
  { key: "exectime",
    name: "exectime",
    kind: "UI",
    default: false,
  },
  { key: "tags",
    name: "tags",
    kind: "UI",
    default: false,
  },
  { key: "debug_msgs",
    name: "debug-msgs",
    kind: "UI",
    default: true,
  },
  /* DEPRECATED
   { key: "io_steps",
    name: "io-steps",
    kind: "UI",
    default: false,
  },
  { key: "target_steps",
    name: "target-steps",
    kind: "UI",
    default: false,
  },*/
  { key: "atomic_substeps",
    name: "atomic-substeps",
    kind: "UI",
    default: false,
  },
  { key: "basic_modules",
    name: "basic-modules",
    kind: "UI",
    default: true,
  }
];
var options = {}; // filled by initOptions, maps key to value
var optionsDefault = {}; // filled by initOptions, maps key to default value


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
    // no codemirrror scroll bars, display full code
    scrollbarStyle: null,
    viewportMargin: Infinity,
  });

  // DEPRECATED:
  // Dimensions for codemirror window, if the code has more lines that the
  // size of the window a scrollbar will appear
  // editor.setSize(700, 600);

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

function scrollToLine(line) { // ({from: from, to: to});
  var vMargin = 100; // pixels
  editor.scrollIntoView(line, vMargin);
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
   // outputFormat: 'line-by-line',
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

var hiddenLines = false;
function hideLines() {
  hiddenLines = true;
  $(".d2h-code-side-line").css("padding", "0");
  $(".d2h-code-side-linenumber").css("display", "none");
//   $(".d2h-code-side-linenumber").html("");
//  $(".d2h-code-side-linenumber").css("width", "0em");
}


//---------------------------------------------------

// Could use Underscore's _escape method.
function escapeHTML(s) {
  return s.replaceAll(/&/g,'&amp;')
          .replaceAll(/</g,'&lt;')
          .replaceAll(/>/g,'&gt;');
}

function newlinetobr(s) {
  return s.trim().replaceAll("\n", "<br/>");
}

function htmlButton(id, label, css, onclick) {
  return "<button id='" + id + "' class='" + css + "' type ='button' onclick='" + onclick + "'>" + label + "</button>";
}

function htmlCheckbox(id, label, css, onclick) {
  return "<label class='checkbox-label'><input id='" + id + "' class='" + css + "' type='checkbox' onclick='" + onclick + "'>" + label + "</label>";
}


function htmlSpan(contents, css) {
  return "<span class='" + css +"'>" + contents + "</span>";
}

var maxButtons = 10;

var curSource = -1;
var curSdiff = -1;
var curBdiff = -1;
var idSourceLeft = -1;
var idSourceRight = -1;
var selectedStep = undefined; // stores a step object

function loadDebugMsgs(messages) {
  var s = '';
  if (options.debug_msgs && messages) {
    for (var i = 0; i < messages.length; i++) {
      s += escapeHTML(messages[i]);
    }
  }
  $('#debugMsgDiv').html(s);
}

function extractShowStep(step) {
  if (step.kind == "Show") {
    return step;
  } else if (step.sub.length == 1) {
    return extractShowStep(steps[step.sub[0]]);
  } else {
    throw new Error("ERROR: extractShowStep: did not find a step-show in depth");
  }
}

function loadDiffForStep(step) {
  // Special case for the diff of steps whose action is a "show" operation,
  // for which we show the diff of the substep
  if (step.tags.includes("show")) {
    try {
      step = extractShowStep(step);
    } catch (error) {
      console.log("ERROR: extractShowStep: did not find a step-show in depth");
    }
  }
  // Print diff for step
  var diffString = "";
  if (step.diff == undefined) {
    // console.log("Diff was not computed for this step");
    $("#debugMsgDiv").html("Diff was not saved for this step; to see it, request the trace of a specific step or set the flag detailed_trace");
  } else if (step.diff == "") {
    $("#debugMsgDiv").html("Diff is empty");
  } else {
    $("#debugMsgDiv").html("");
    diffString = step.diff;
  }
  loadDiffFromString(diffString);
}

function loadDiffFromString(diffString) {
  // this function should be called only after DOM contents is loaded
 var targetElement = document.getElementById("diffDiv");
 var diff2htmlUi = new Diff2HtmlUI(targetElement, diffString, configuration);
 diff2htmlUi.draw();
 diff2htmlUi.highlightCode();

 const reg1 = /<del>([\s\n]*)/g
 $('.d2h-code-line-ctn').each(function() {
  $(this).html( $(this).html().replace(reg1, "$1<del>") );
});

const reg2 = /<ins>([\s\n]*)/g
$('.d2h-code-line-ctn').each(function() {
 $(this).html( $(this).html().replace(reg2, "$1<ins>") );
});

if (options.compact) {
  const reg3 = /  /g
  $('.d2h-code-line-ctn').each(function() {
    $(this).html( $(this).html().replace(reg3, " ") );
  });
}

 // identify the two sides of the diff, and register handlers for click on the line numbers;
 $('.d2h-file-side-diff').first().addClass('diffBefore');
 $('.d2h-file-side-diff').last().addClass('diffAfter');
 $('.diffBefore .d2h-code-side-linenumber').click(function(e) {
     var line = e.target.outerText; loadSourceAtLine(selectedStep.ast_before, line); });
 $('.diffAfter .d2h-code-side-linenumber').click(function(e) {
     var line = e.target.outerText; loadSourceAtLine(selectedStep.ast_after, line); });

  // if hideLines() has been called once, call it again
  if (hiddenLines) {
    hideLines();
  }
}

function resetView() {
  /*$("#sourceDiv").hide();
  $("#diffDiv").hide();
  $("#detailsDiv").html("");
  $("#infoDiv").html("");*/
  curSource = -1;
  curSdiff = -1;
  curBdiff = -1;
  $(".ctrl-button").removeClass("ctrl-button-selected ctrl-button-covered");
}

function getBdiffCovering(sdiffId) {
  for (var i = 0; i < bigsteps.length; i++) {
    if (bigsteps[i].start <= sdiffId && sdiffId < bigsteps[i].stop) {
      return i;
    }
  }
  return -1; // should not happen
}

function showOrHide(obj, visible) {
  if (visible) {
    obj.show();
  } else {
    obj.hide();
  }
}

function hideNoncoveredButtons(bdiffId) {
  // if (smallsteps.length <= maxButtons)
  //  return; // no hiding needed if only a few steps
  var step = bigsteps[bdiffId];
  var start = step.start;
  var stop = step.stop;
  /*for (var i = 0; i <= codes.length; i++) {
    // DEPRECATED showOrHide($("#button_code_" + i), (start <= i && i <= stop));
    if (i < smallsteps.length)
      showOrHide($("#button_sdiff_" + i), (start <= i && i < stop));
  }*/
  for (var i = 0; i < smallsteps.length; i++) {
    showOrHide($("#button_sdiff_" + i), (start <= i && i < stop));
  }
}

function showBdiffCovered(sdiffId) {
  var bdiffId = getBdiffCovering(sdiffId);
  if (bdiffId == -1)
    return;
  $("#button_bdiff_" + bdiffId).addClass("ctrl-button-covered");
  hideNoncoveredButtons(bdiffId);
}

function displayInfo(descr) {
  $("#infoDiv").html(descr);
}

function loadSourceAtLine(sourceCode, line) {
  loadSource(sourceCode);
  // TODO: autoscrolling does not work...
  var delay = 1000; // milliseconds
   setTimeout(function() { scrollToLine(line); },
   delay);
}

function loadSource(sourceCode, dontResetView) {
  if (! dontResetView) {
    resetView();
  }
  $("#sourceDiv").show();
  if (sourceCode == undefined) {
    sourceCode = "";
    // console.log("AST was not saved for this step");
    // $("#debugMsgDiv").html("AST was not saved for this step");
    sourceCode = "// AST was not saved for this step; to see it, request the trace of a specific step or set the flag detailed_trace";
  }
  editor.setValue(sourceCode);
  // DEPRECTED $("#button_code_" + id).addClass("ctrl-button-selected");
  // showBdiffCovered(id);
  //curSource = id;
}

function loadSdiff(id) {
  $('#button_sdiff_next').focus();
  resetView();
  $("#diffDiv").show();
  var step = smallsteps[id];
  selectedStep = step;
  reloadTraceView(); //loadStepDetails(step.id);
  loadDiffForStep(step);
  var sStep = htmlSpan(newlinetobr(escapeHTML(step.script)), "step-info");
  if (options.exectime) {
    var nbMilliseconds = Math.round(1000 * step.exectime);
    var sTime = htmlSpan(nbMilliseconds + "ms", "timing-info") + "<div style='clear: both'></div>";
    sStep += sTime;
  }
  displayInfo(sStep);
  $("#button_sdiff_" + id).addClass("ctrl-button-selected");
  // DEPRECATED $("#button_code_" + id).addClass("ctrl-button-covered");
  // DEPRECATED $("#button_code_" + (id+1)).addClass("ctrl-button-covered");
  showBdiffCovered(id);
  curSdiff = id;
  idSourceLeft = id;
  idSourceRight = id+1;
}

function loadBdiff(id) {
  $('#button_bdiff_next').focus();
  resetView();
  $("#diffDiv").show();
  var step = bigsteps[id];
  selectedStep = step;
  reloadTraceView(); // loadStepDetails(step.id);
  loadDiffForStep(step);
  $("#button_bdiff_" + id).addClass("ctrl-button-selected");
  var sStep = htmlSpan(escapeHTML(step.script), "step-info");
  displayInfo(sStep);
  curBdiff = id;
  // $("#button_sdiff_" + bigsteps[id].start).addClass("ctrl-button-covered");
  hideNoncoveredButtons(id);
  curSdiff = bigsteps[id].start - 1; // -1 to anticipate for "next" being pressed
  idSourceLeft = bigsteps[id].start;
  idSourceRight =bigsteps[id].stop;
}

// LATER: simplify, as curSource is deprecated
function nextSdiff() {
  /*if (curSdiff == -1 && curSource != -1) {
    curSdiff = curSource - 1;
  }*/
  //var id = Math.min(curSdiff + 1, smallsteps.length-1);
  var id = (curSdiff + 1) % smallsteps.length;
  loadSdiff(id);
}

// LATER: simplify, as curSource is deprecated
function nextBdiff() {
  /*if (curBdiff == -1) {
    if (curSdiff == -1 && curSource != -1) {
      curSdiff = curSource - 1;
    }
    curBdiff = getBdiffCovering(curSdiff) - 1; // anticipate for the +1 operation
  }
  //var id = Math.min(curBdiff + 1, bigsteps.length-1);
  if (curBdiff == -1) {
    curBdiff = 0;
  }*/
  var id = (curBdiff + 1) % bigsteps.length;
  loadBdiff(id);
}

// handles a click on a step bullet item, to focus on that step
function focusOnStep(idStep) {
  //console.log("focusOnStep " + idStep + " with parent " + steps[idStep].parent_id);
  options["hide_substeps"] = false;
  optionsCheckboxUpdate();
  resetView();
  var step = steps[idStep];
  selectedStep = step;
  reloadTraceView();
  if (step.hasOwnProperty("smallstep_id")) {
    //console.log("reload small step " + steps[step.smallstep_id].id)
    loadSdiff(step.smallstep_id);
  } else if (step.hasOwnProperty("bigstep_id")) {
    //console.log("reload big step " + steps[step.bigstep_id].id)
    loadBdiff(step.bigstep_id);
  }

}

// handles a click on a step, to view details
function loadStepDetails(idStep) {
  var step = steps[idStep];

  $(".tree-step").removeClass("step-selected");
  $("#tree-step-" + idStep).addClass("step-selected");

  if (options.ast_before || options.ast_after) {
    var ast = (options.ast_before) ? step.ast_before : step.ast_after;
    loadSource(ast, true);
    $("#debugMsgDiv").html("")
    $("#diffDiv").hide();
    $("#statsDiv").hide();
    $("#sourceDiv").show();
  } else if (/* options.stats */ false) {
    let visitedSteps = new Set();
    $("#statsDiv").html(stepToHTMLStats(step, true, visitedSteps));
    $("#diffDiv").hide();
    $("#statsDiv").show();
    $("#sourceDiv").hide();
  } else {
    loadDebugMsgs(step.debug_msgs);
    loadDiffForStep(step);
    $("#diffDiv").show();
    $("#statsDiv").hide();
    $("#sourceDiv").hide();
  }

  /*
  if (step.kind == "Target") {
    $("#diffDiv").show();
    //loadSource(step.ast_after, true);
  } else {
    $("#diffDiv").hide();
    // loadSource(step.ast_before, true);
    loadSource(step.ast_before, true);
  }
  */
}

function stepToHTML(step, isOutermostLevel) {
  if (options["hide-same-code"] && step.tags.includes("same-code")) {
    return "";
  }
  if (! options.step_change && step.kind == "Change") {
    return "";
  }
  if (options["hide_substeps"]
    && step.kind != "Root"
    && step.kind != "Big"
    && step.kind != "Small") {
    return "";
  }

  // console.log("steptohtml " + step.id);
  var s = "";

  // Recursive steps
  var sSubs = "";
  const showSubsteps =
    (options.atomic_substeps || !step.tags.includes("atomic"));
  if (showSubsteps) {
    for (var i = 0; i < step.sub.length; i++) {
      var substep = steps[step.sub[i]];
      sSubs += stepToHTML(substep, false)
    }
  }

  const hideStep = (step.tags.some((tag) => options["hide-" + tag]));
  // DEPRECATED
    // (!options.target_steps && step.kind == "Target") ||
    // (!options.io_steps && step.kind == "IO") ||
  var isRoot = (step.kind == "Root");
   // (step.id == 0);

  /*if (isRoot) {
    return "<ul class='step-sub'> " + sSubs + "</ul>\n";
  } else */ if (hideStep) {
    return sSubs;
  }

  // TODO: display check_validity

  var lineClass = "";
  if (step.kind == "IO" || step.kind == "Target") {
    lineClass = "step-io-target";
  } else if (step.kind == "Error") {
    lineClass = "step-error";
  }
  var sTime = "";
  if (options.exectime) {
    var t = 1000 * step.exectime; // milliseconds
    var nb = "";
    if (t > 10) {
      nb = Math.round(t);
    } else if (t > 1) {
      nb = Math.round(10* t) / 10;
    } else {
      nb = Math.round(100 * t) / 100;
    }
    if (t < 0.1) { // don't display < 0.1ms
      sTime = "";
    } else {
      sTime = "" + nb + "ms";
    }
    var sTimeClass = "exectime-small";
    if (t > 50) {
      sTimeClass = "exectime-heavy";
    } else if (t > 10) {
      sTimeClass = "exectime-mid";
    }
    sTime = "<span class='" + sTimeClass + "'>" + sTime + "</span>";
  }

  var sHasMsg = "";
  if (step.debug_msgs && step.debug_msgs.length > 0) {
    sHasMsg = "<span class='has-debug-msg'>MSG</span>";
  }

  var sKind = "";
  if (step.script_line !== undefined) {
    sKind = " [<b>" + step.script_line + "</b>] ";
  } else if (step.kind == "Transfo") {
    sKind = "";
  } else if (!options.compact) {
    sKind = " [" + escapeHTML(step.kind) + "] ";
  }
  var sScript = escapeHTML(step.script);
  if (step.kind == "Big") {
    sScript = "<b>Bigstep: " + sScript + "</b>";
  }
  var sOnClick = "";
  if (step.hasOwnProperty("id")) { // LATER: refine
    sOnClick = "onclick='loadStepDetails(" + step.id + ")'";
  }
  var sTags = "";
  if (options.tags) {
    sTags += " Tags:[";
    for (var t = 0; t < step.tags.length; t++) {
      sTags += step.tags[t] + ",";
    }
    sTags += "]";
  }
  var sName = escapeHTML(step.name);
  if (!options.basic_modules) {
    sName = sName.replace(/_basic/,'');
  }
  sName = sName.replace(/_loop_list/,'');

  var sArgs = "";
  if (options.args && step.args) {
    for (var i = 0; i < step.args.length; i++) {
      var arg = step.args[i];
      if (arg.name == "") {
        sArgs += " " + arg.value;
      } else {
        sArgs += " " + arg.name + ":" + arg.value;
      }
    }
    sArgs = "<span class='args'>" + escapeHTML(sArgs) + "</span>";
  }

  // Link to focus on step and to exit current focus // root is its own parent
  var idOnClickFocusOnStep = (isOutermostLevel) ? step.parent_id : step.id;
  var sOnClickFocusOnStep = "onclick='focusOnStep(" + idOnClickFocusOnStep + ")'";

  // Line symbol
  var sStepSymbol;
  if (step.isvalid) {
    sStepSymbol = "&#10004;" // check
  } else if (step.has_valid_parent) {
    sStepSymbol = "&diams;"; // diamond, validity is covered by a parent
  } else if (! step.check_validity) {
    sStepSymbol = "&#9679;" // circle, was not trying to check validity
    // lineClass = "step-has-valid_parent";
  } else { // step.check_validity && ! step.isvalid
    sStepSymbol= "&#10060"; // cross, for a trustme step
    lineClass = "step-invalid";
  }

  // Line color
  var fullLineClass = "";
  if (step.kind == "Big") {
    fullLineClass = "step-big";
  } else if (step.kind == "Small") {
    if (step.tags.includes("show")) {
      lineClass = "step-show";
    } else if (step.isvalid) {
      lineClass = "step-valid";
    }
  }
  if (step.diff == undefined) {
    lineClass += " step-nodiff";
  }

  // Line contents
  if (! isRoot) {
    s += "<div id='tree-step-" + step.id + "' class='tree-step " + fullLineClass + "'><span class='step-bullet' " + sOnClickFocusOnStep + ">" + sStepSymbol +"</span><span " + sOnClick + " class='step-title " + lineClass + "'>" + sTime + sKind + sHasMsg + sName + sArgs + " " + sScript + sTags + "</span></div>";
  }

  if (options.justif) {
    for (var i = 0; i < step.justif.length; i++) {
      s += "<div class='step-justif-text'>" + escapeHTML(step.justif[i]) + "</div>"
    }
  }

  // Substeps
  s += "<ul class='step-sub'> " + sSubs + "</ul>\n";

  if (isOutermostLevel) {
    return s;
  } else {
    return "<li>" + s + "</li>\n";
  }
}

// TODO: factorize with stepToHTML?
function stepToHTMLStats(step) {
  let visitedSteps = new Set();
  visitSteps(step, visitedSteps);
  return "<ul>" + [...visitedSteps].sort().map(x => "<li>" + x + "</li>").join('') + "</li>";
}

function visitSteps(step, visitedSteps) {
  if (!options.noop_steps && (step.ast_before == step.ast_after)) {
    // TODO: precompute '==' somewhere
    return "";
  }

  const showSubsteps =
    (options.atomic_substeps || !step.tags.includes("atomic"));
  if (showSubsteps) {
    for (var i = 0; i < step.sub.length; i++) {
      var substep = steps[step.sub[i]];
      visitSteps(substep, visitedSteps);
    }
  }

  var sName = escapeHTML(step.name);
  if (!options.basic_modules) {
    sName = sName.replace(/_basic/,'');
  }

  const hideStep =
    (visitedSteps.has(sName)) ||
    (step.kind != "Transfo") ||
    (step.tags.some((x) => hideTags.has(x)));
  if (hideStep) {
    return;
  }

  visitedSteps.add(sName);
}

function reloadTraceView() {
  //console.log("reloadTraceView " + selectedStep.id);
  // var shouldShowDetails = ($("#detailsDiv").html() == "");
  resetView();
  if (options.tree) {
    $("#detailsDiv").show();
  } else {
    $("#detailsDiv").hide();
  }
  if (typeof selectedStep !== "undefined") {
    loadStepDetails(selectedStep.id); // TODO inline here?
    $("#detailsDiv").html(stepToHTML(selectedStep, true));
  }

  //if (shouldShowDetails) {
  //  $("#diffDiv").hide();

  //} else {
  //  $("#detailsDiv").html("");
  //}
}

// handles click on the "all" button
function viewDetailsAll() {
  // $("#diffDiv").hide();
  selectedStep = steps[0]; // root
  reloadTraceView();
  // $("#detailsDiv").html(stepToHTML(selectedStep));
}

// handles update after click on "normal" or "full" button
function optionsCheckboxUpdate() {
  // update checkbox display // TODO: use this also in other place
  for (var key in options) {
    $('#option_' + key).prop('checked', options[key]);
  }
}

// handles click on the "normal" button
function viewDetailsNormal() {
  for (var key in options) {
    options[key] = optionsDefault[key];
  }
  optionsCheckboxUpdate();
  reloadTraceView();
}

// handles click on the "full" button
function viewDetailsFull() {
  for (var key in options) {
    options[key] = false;
  }
  options["tree"] = true;
  options["args"] = true;
  options["justif"] = true;
  options["exectime"] = true;
  options["step_change"] = true;
  optionsCheckboxUpdate();
  reloadTraceView();
}

function initOptions() {
  for (var i = 0; i < optionsDescr.length; i++) {
    var descr = optionsDescr[i];
    optionsDefault[descr.key] = descr.default;
    options[descr.key] = descr.default;
  }
}

initOptions();
function initControls() {
  var s = "";
  function addRow(sTitle, sRow) {
    s += "<span class='row-title'>" + sTitle + ":</span>" + sRow + "<br class='row-br'/>";
  };

  // Code buttons
  /* DEPRECATED
  var sCode = "";
  sCode += htmlButton("button_code_next", "next", "next-button", "nextSource()");
  for (var i = 0; i < codes.length; i++) {
    sCode += htmlButton("button_code_" + i, i, "ctrl-button", "loadSource(" + i + ")");
  }
  addRow("Source", sCode);
  */

  // Big diff buttons
    var sBdiff = "";
  sBdiff += htmlButton("button_bdiff_next", "next", "next-button", "nextBdiff()");
  for (var i = 0; i < bigsteps.length; i++) {
    sBdiff += htmlButton("button_bdiff_" + i, (i+1), "ctrl-button", "loadBdiff(" + i + ")");
  }
  if (bigsteps.length > 1) {
    addRow("BigSteps", sBdiff);
  }

  // Small diff buttons
  var sSdiff = "";
  sSdiff += htmlButton("button_sdiff_next", "next", "next-button", "nextSdiff()");
  for (var i = 0; i < smallsteps.length; i++) {
    sSdiff += htmlButton("button_sdiff_" + i, (i+1), "ctrl-button", "loadSdiff(" + i + ")");
  }
  addRow("SmallSteps", sSdiff);

  // Details button
  // s += htmlButton("button_details", "details", "details-button", "toggleDetails()");
  s += htmlButton("button_all", "all", "details-button", "viewDetailsAll()");

  // Generate checkboxes
  for (var i = 0; i < optionsDescr.length; i++) {
    var descr = optionsDescr[i];
    var id = "option_" + descr.key;
    s += htmlCheckbox(id, descr.name, "details-checkbox", "updateOptions()");
  }

  // Full/normal button
  s += htmlButton("button_normal", "normal", "details-button", "viewDetailsNormal()");
  s += htmlButton("button_full", "full", "details-button", "viewDetailsFull()");

  $("#contents").html(s);

  // initialize checkboxes
  for (var i = 0; i < optionsDescr.length; i++) {
    var descr = optionsDescr[i];
    var id = "option_" + descr.key;
    $('#' + id).prop('checked', options[descr.key]);
  }
}

// handles modification of options by click on the checkboxes
function updateOptions() {
  var ast_before_was_checked = options.ast_before;
  for (var i = 0; i < optionsDescr.length; i++) {
    var descr = optionsDescr[i];
    var id = "option_" + descr.key;
    options[descr.key] = $('#' + id).prop('checked');
  }
  /* LATER
  if (options.exectime) {
    options["hide-IO"] = false;
    $('#option_hide-IO').prop('checked', false);
  }*/
  if (options.ast_before && options.ast_after) {
    if (ast_before_was_checked) {
      options.ast_before = false;
      $('#option_ast_before').prop('checked', options.ast_before);
    } else {
      options.ast_after = false;
      $('#option_ast_after').prop('checked', options.ast_after);
      // LATER: a function to change an option and the checkbox
    }
  }
  reloadTraceView();
}

function initSteps() {
  // reads global variable [steps]
  // writes global variables [smallsteps] and [bigsteps] and [hasBigsteps]
  var rootStep = steps[0];

  var rootSub = rootStep.sub;
  if (rootSub.length == 0) {
    console.log("Error: no steps in tree")
    return;
  }
  // set global variable [hasBigsteps]
  hasBigsteps = false;
  for (var i = 0; i < rootSub.length; i++) {
    if (steps[rootSub[i]].kind == "Big") {
      hasBigsteps = true;
      break;
    }
  }

  // counter used to fill [smallsteps] array
  var curSmallStep = 0;

  // function to iterate over an array of step ids treated as small steps
  function numberSmallSteps(stepIds) {
    for (var i = 0; i < stepIds.length; i++) {
      var smallstep_id = stepIds[i];
      var smallstep = steps[smallstep_id];
      if (smallstep.kind == "IO") {
        continue;
      }
      if (smallstep.kind != "Small") {
        console.log("Error: numberSmallSteps expected a small step but encountered a step of kind " + smallstep.kind);
      }
      // DEPRECATED(redundant) smallstep.id = smallstep_id;
      smallstep.smallstep_id = curSmallStep;
      smallsteps[curSmallStep] = smallstep;
      curSmallStep++;
    }
  };

  if (hasBigsteps) {
    // filling of [bigsteps] and [smallsteps] array
    for (var i = 0; i < rootSub.length; i++) {
      var bigstep_id = rootSub[i];
      var bigstep = steps[bigstep_id];
      if (bigstep.kind == "IO") {
        continue;
      }
      if (bigstep.kind != "Big") {
        console.log("Error: initSteps expected a big-step but encountered a step of kind " + bigstep.kind);
      }
      // DEPRECATED(redundant) bigstep.id = bigstep_id;
      bigstep.start = curSmallStep;
      numberSmallSteps(bigstep.sub);
      bigstep.stop = curSmallStep;
      bigstep.bigstep_id = i;
      bigsteps[i] = bigstep;
    }
  } else {
    // there are no big-steps, filling only [smallsteps] array
    numberSmallSteps(rootSub);
  }
}

function initTree(id, parent_id, has_valid_parent) {
  var step = steps[id];
  step.parent_id = parent_id;
  step.has_valid_parent = has_valid_parent;
  for (var j = 0; j < step.sub.length; j++) {
    initTree(step.sub[j], id, (has_valid_parent || step.isvalid));
  }
}


function initAllTags() {
  // fills the object allTags with keys that correspond to all possible tags
  for (var i = 0; i < steps.length; i++) {
    var tags = steps[i].tags;
    for (var t = 0; t < tags.length; t++) {
      var tag = tags[t];
      allTags[tag] = true;
    }
  }
  // LATER: organize known tags to the front
  // completes the options array with one entry per tag
  for (tag in allTags) {
    var val =
      (optionsDefaultValueForTags.hasOwnProperty(tag))
      ? optionsDefaultValueForTags[tag]
      : false;
    var key = "hide-" + tag;
    var descr = {
      key: key,
      name: key,
      kind: "UI",
      default: val,
    };
    optionsDescr.push(descr);
    options[key] = val;
  }
}

document.addEventListener('DOMContentLoaded', function () {
  var isRootedTrace = (steps[0].kind == "Root");
  initEditor();
  if (isRootedTrace) {
    initSteps();
  }
  initAllTags();
  initOptions();
  initControls();
  initSplitView();
  initTree(0, 0, false); // the root is its own parent, has no valid parent
  // editor.setValue("click on a button");
  if (isRootedTrace) {
    /*if (hasBigsteps) {
      loadBdiff(0);
    } else {
      loadSdiff(0);
    }
    DEPRECATED
    */
    $('#button_bdiff_next').focus();
  } else {
    $('#button_sdiff_next').hide();
    $('.row-title').hide();
    $('.row-br').hide();
  }

  // start by showing the tree of steps on the root, or the requested step
  var stepInit = 0; // root
  if (typeof startupOpenStep !== "undefined") {
    stepInit = startupOpenStep;
  }
  selectedStep = steps[stepInit];
  reloadTraceView(); // calls loadStepDetails(selectedStep)
});

// alternative:
// but there could be many lines..


//// vertical split resizing

var resizer, leftSplit, rightSplit;

function initSplitView() {
  resizer = document.getElementById('vSplitDiv');
  leftSplit = resizer.previousElementSibling;
  rightSplit = resizer.nextElementSibling;
  resizer.addEventListener('mousedown', mouseDownHandler);
}

// The current position of mouse
let x = 0;
let y = 0;

// Width of left side
let leftWidth = 0;

// Handle the mousedown event
// that's triggered when user drags the resizer
const mouseDownHandler = function (e) {
    // Get the current mouse position
    x = e.clientX;
    y = e.clientY;

    leftWidth = leftSplit.getBoundingClientRect().width;

    // Attach the listeners to `document`
    document.addEventListener('mousemove', mouseMoveHandler);
    document.addEventListener('mouseup', mouseUpHandler);
};

const mouseMoveHandler = function (e) {
  // How far the mouse has been moved
  const dx = e.clientX - x;
  const dy = e.clientY - y;

  const newLeftWidth = ((leftWidth + dx) * 100) / resizer.parentNode.getBoundingClientRect().width;
  leftSplit.style.width = `${newLeftWidth}%`;

  document.body.style.cursor = 'col-resize';
  leftSplit.style.userSelect = 'none';
  leftSplit.style.pointerEvents = 'none';

  rightSplit.style.userSelect = 'none';
  rightSplit.style.pointerEvents = 'none';
};

const mouseUpHandler = function () {
  resizer.style.removeProperty('cursor');
  document.body.style.removeProperty('cursor');

  leftSplit.style.removeProperty('user-select');
  leftSplit.style.removeProperty('pointer-events');

  rightSplit.style.removeProperty('user-select');
  rightSplit.style.removeProperty('pointer-events');

  // Remove the handlers of `mousemove` and `mouseup`
  document.removeEventListener('mousemove', mouseMoveHandler);
  document.removeEventListener('mouseup', mouseUpHandler);
};
