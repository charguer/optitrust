// The function [Trace.dump_trace_to_js] in [trace.ml] documents the data representation
// used for the trace. It looks like this, where 0 is always the id of the root step.

// TODO: restore the feature of displaying sources at lines

/*
var startupOpenStep = 45; // optional binding
var webview = true; // optional binding
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
   script_line_start: 23, // possibly undefined
   script_line_stop: 24, // possibly undefined
   code_before: window.atob("..."), // NOT YET IMPLEMENTED; could also an id of an source code stored in a different array, for improved factorization
   code_after: window.atob("..."), // NOT YET IMPLEMENTED
   diff: window.atob("..."), // could be slow if requested for all!
   sub: [ j1, j2, ... jK ]  // ids of the sub-steps
   }
step[1] = ...

Additional fields are set by the function   initTree
- parent_id
- has_valid_parent

*/

// Flag indicating whether the trace has been produced with [Flag.check_validity = true]
var root_checking_validity = undefined;

var debug_html_view = true;

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
  { key: "advanced",
    name: "advanced",
    kind: "tree",
    default: false
  },
  { key: "hide_empty_diff",
    name: "hide-empty-diff",
    kind: "advanced",
    default: true,
  },
  /*,*/
/*  { key: "stats",
    name: "stats",
    kind: "standard",
    default: false,
  }, */
  { key: "kind",
    name: "kind",
    kind: "advanced",
    default: false,
  },

  { key: "step_change",
    name: "step-change",
    kind: "advanced",
    default: false,
  },
  { key: "args",
    name: "arguments",
    kind: "tree",
    default: false,
  },
  { key: "justif",
    name: "justification",
    kind: "tree",
    default: false,
  },
  /* always-true
  { key: "simpl-show",
    name: "simpl-show-steps",
    kind: "standard",
    default: true,
  },*/
  { key: "exectime",
    name: "exectime",
    kind: "advanced",
    default: false,
  },
  { key: "tags",
    name: "tags",
    kind: "advanced",
    default: false,
  },
  { key: "debug_msgs",
    name: "debug-msgs",
    kind: "advanced",
    default: true,
  },
  /* DEPRECATED
   { key: "io_steps",
    name: "io-steps",
    kind: "standard",
    default: false,
  },
  { key: "target_steps",
    name: "target-steps",
    kind: "standard",
    default: false,
  },*/
  { key: "atomic_substeps",
    name: "atomic-substeps",
    kind: "advanced",
    default: false,
  },
  { key: "basic_modules",
    name: "basic-modules",
    kind: "advanced",
    default: true,
  },
  { radio: "view",
    value: "diff",
    name: "Diff",
    kind: "ast",
    default: !debug_html_view,
  },
  { radio: "view",
    value: "code_before",
    name: "Code before",
    kind: "ast",
    default: debug_html_view,
  },
  { radio: "view",
    value: "code_after",
    name: "Code after",
    kind: "ast",
    default: false,
  },
  { key: "decode",
    name: "Decode",
    kind: "serialized_ast",
    default: true,
  },
  { radio: "typing_style",
    value: "hide",
    name: "Hide res",
    kind: "serialized_ast",
    default: false,
  },
  { radio: "typing_style",
    value: "annot",
    name: "Res annot",
    kind: "serialized_ast",
    default: !debug_html_view,
  },
  { radio: "typing_style",
    value: "ctx",
    name: "Res context",
    kind: "serialized_ast",
    default: false,
  },
  { radio: "typing_style",
    value: "usage",
    name: "Res usage",
    kind: "serialized_ast",
    default: false,
  },
  { radio: "typing_style",
    value: "full",
    name: "Full res",
    kind: "serialized_ast",
    default: false,
  },
  { radio: "typing_style",
    value: "html",
    name: "Interactive",
    kind: "serialized_ast",
    default: debug_html_view,
  },
  { key: "compact",
    name: "Compact",
    kind: "ast",
    default: true,
  },
];

var options = {}; // filled by initOptions, maps key to value
var optionsDefault = {}; // filled by initOptions, maps key to default value

var expanded = []; // maps node ids to true, false or "full", indicating if node is expanded;
                   // entries in this map are optional for nodes
var hasErrorSubstep = []; // maps node ids to boolean indicating if the node contains an error

// given the value of a 'radio' field, return the key of the activated option having this 'radio' field.
function getRadioOption(radio) {
  for (var i = 0; i < optionsDescr.length; i++) {
    var descr = optionsDescr[i];
    if (descr.radio && descr.radio == radio && options[descr.key]) {
      return descr.value;
    }
  }
}


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

function htmlRadio(id, label, css, radio_name, onclick) {
  return "<label class='checkbox-label'><input id='" + id + "' class='" + css + "' type='radio' name='"+radio_name+"' onclick='" + onclick + "'>" + label + "</label>";
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

var selectedStep = undefined; // stores the id of the selected step

function loadStepDebugMsgs(messages) {
  var s = '';
  if (options.debug_msgs && messages) {
    for (var i = 0; i < messages.length; i++) {
      s += escapeHTML(messages[i]);
    }
  }
  $('#stepMsgDiv').html(s);
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

 /* Currently, this is a buggy feature: there is no code to jump to the relevant line, reactivate if there is a workaround
 // identify the two sides of the diff, and register handlers for click on the line numbers;
 $('.d2h-file-side-diff').first().addClass('diffBefore');
 $('.d2h-file-side-diff').last().addClass('diffAfter');
 $('.diffBefore .d2h-code-side-linenumber').click(function(e) {
     var line = e.target.outerText; loadSourceAtLine(selectedStep.code_before, line); });
 $('.diffAfter .d2h-code-side-linenumber').click(function(e) {
     var line = e.target.outerText; loadSourceAtLine(selectedStep.code_after, line); });
  */

  // if hideLines() has been called once, call it again
  if (hiddenLines) {
    hideLines();
  }
}

function resetView() {
  /*$("#sourceDiv").hide();
  $("#diffDiv").hide();
  $("#treeDiv").html("");
  $("#infoDiv").html("");*/
  curSource = -1;
  curSdiff = -1;
  curBdiff = -1;
  /*$(".ctrl-button").removeClass("ctrl-button-selected ctrl-button-covered");*/
}

function showOrHide(obj, visible) {
  if (visible) {
    obj.show();
  } else {
    obj.hide();
  }
}

function displayInfo(descr) {
  $("#infoDiv").html(descr);
}

/*function loadSourceAtLine(sourceCode, line) {
  loadSource(sourceCode);
  // TODO: autoscrolling does not work...
  var delay = 1000; // milliseconds
   setTimeout(function() { scrollToLine(line); },
   delay);
}*/

function loadSource(sourceCode) {
  $("#sourceDiv").show();
  editor.setValue(sourceCode);
}

function loadSourceInHtml(sourceCodeInHtml) {
  $("#sourceInHtmlDiv").show();
  $("#sourceInHtmlDiv_source").html(sourceCodeInHtml);
  //$("#sourceInHtmlDiv_source").html(sourceCodeInHtml);
  console.log({foo: sourceCodeInHtml});
  $(".c_instr").click(function(event) {
    var elem = event.target;
    loadSourceInHtmlDetails(elem);
    console.log(elem.id);
    });
}

function loadSourceInHtmlDetails(htmlElem) {
  // copy the selected instruction into the details area, which uses a different css
  $("#sourceInHtmlDiv_infos").html(htmlElem.outerHTML);
}

function exandClickHandler(event) {
  console.log(event);
}

// handles click or ctrl+click or shift+click or shift+ctrl+click
function expandClick(event, idStep) {
  //console.log(event);
  const expandHidden = event.shiftKey;
  if (event.ctrlKey) {
    // ctrl+click expands recursively
    // shift+ctrl+click fully expands recursively
    toggleExpandStep(idStep, expandHidden);
    expandRecursively(idStep, expanded[idStep]);
  } else {
    // click expands or collapse
    // shift+click expands at depth one, even hidden substeps
    toggleExpandStep(idStep, expandHidden);
  }

  reloadTraceView(); // LATER: could be smarter to redraw only the subtree involved
}

// handles a click on a step expand control
function toggleExpandStep(idStep, expandHidden) {
  //console.log("toggleExpandStep " + idStep + " with parent " + steps[idStep].parent_id);
  //var step = steps[idStep];

  if (expandHidden) {
    if (expanded[idStep] == 'full') {
      expanded[idStep] = true;
    } else {
      expanded[idStep] = 'full'
    }
  } else {
    // toggle expansion and update tree view
    expanded[idStep] = !expanded[idStep];
  }
}

// handles a [shift]+[ctrl]+click on a step expand control;
// If depth is an int, it serves as a bound;
// if depth is a kind (e.g. 'Root', 'Small' or 'Big),
// thn it expands until reaching that kind.
// If expandHidden is true, then isStepHidden(step) is ignored.
function expandRecursively(idStep, expandValue, depth) {
  var step = steps[idStep];
  if (depth === -1 || step.kind === depth)
    return;
  var depthForSub = depth;
  if (Number.isInteger(depth)) {
    depthForSub = depth-1;
  }

  if (expandValue != 'full' && isStepHidden(step)) {
    return; // don't expand hidden steps
  }
  expanded[idStep] = expandValue;
  if (!options.atomic_substeps || !step.tags.includes("atomic")) {
    for (var i = 0; i < step.sub.length; i++) {
      var idSubstep = step.sub[i];
      expandRecursively(idSubstep, expandValue, depthForSub);
    }
  }
}

// expand all paths that lead to an error,
// the function returns a boolean indicating if a sub-stepn has an error in it
function checkErrorSubstep(idStep) {
  var step = steps[idStep];
  var isError = (step.kind == "Error");
  hasErrorSubstep[idStep] = isError;
  for (var i = 0; i < step.sub.length; i++) {
    var idSubstep = step.sub[i];
    var isErrorSubstep = checkErrorSubstep(idSubstep);
    hasErrorSubstep[idStep] = hasErrorSubstep[idStep] || isErrorSubstep;
  }
  return hasErrorSubstep[idStep];
}

// handles a click on a step, to view details
function loadStepDetails(idStep) {
  selectedStep = idStep;
  var step = steps[idStep];

  $(".tree-step").removeClass("step-selected");
  $("#tree-step-" + idStep).addClass("step-selected");

  loadStepDebugMsgs(step.debug_msgs);

  // Special case for the diff of steps whose action is a "show" operation,
  // for which we show the diff of the substep
  if (step.tags.includes("show")) {
    try {
      step = extractShowStep(step);
    } catch (error) {
      console.log("ERROR: extractShowStep: did not find a step-show in depth");
    }
  }

  // Get view mode and style
  let view = getRadioOption("view");
  let typing_style = getRadioOption("typing_style");

  // Print diff for step
  var stepCode;
  if (serialized_trace) {
    // We have a serialized trace server, get the step details from there
    stepCode = fetch(serialized_trace + `?view=${view}&step=${step.id + root_serialized_step_id}&decode=${options.decode}&typing_style=${typing_style}&timestamp=${serialized_trace_timestamp}`)
      .then((response) => {
        if (response.status == 419) {
          window.location.reload();
          throw new Error(`Trace is out of date, reloading page`);
        }
        else if(!response.ok) {
        return response.text().then((error) => {
            throw new Error(`Failed to retreive data: ${error}`);
          });
        }
        return response.text();
      });
  }
  else {
    // No trace server: get the step details directly from the step object or fail
    if (view == "diff") {
      if (step.diff == undefined) {
        stepCode = Promise.reject(new Error("Diff was not computed for this step and there is no serialized trace"))
      } else {
        stepCode = Promise.resolve(step.diff);
      }
    } else if (view == "code_before") {
      if (step.code_before == undefined) {
        stepCode = Promise.reject(new Error("Code before this step was not saved, request the trace of a specific step or set the flag detailed_trace"))
      } else {
        stepCode = Promise.resolve(step.code_before);
      }
    } else if (view == "code_after") {
      if (step.code_after == undefined) {
        stepCode = Promise.reject(new Error("Code after this step was not saved, request the trace of a specific step or set the flag detailed_trace"))
      } else {
        stepCode = Promise.resolve(step.code_after);
      }
    } else {
      stepCode = Promise.reject(new Error(`View mode ${view} is not included in standalone traces`))
    }
  }
  $("#debugMsgDiv").html("");
  $("#diffDiv").hide();
  $("#statsDiv").hide();
  $("#sourceDiv").hide();
  $("#sourceInHtmlDiv").hide();
  stepCode
    .then((code) => {
      if (view == "diff") {
        if (code == "") {
          throw new Error("Diff is empty");
        } else {
          $("#diffDiv").show();
          loadDiffFromString(code);
        }
      } else if (view == "code_before" || view == "code_after") {
        if (typing_style == "html") {
          loadSourceInHtml(code);
        } else {
          loadSource(code);
        }
      } else {
        console.log("invalid view");
      }
    })
    .catch((err) => {
      $("#debugMsgDiv").html(err.message);
    });
}

function isStepHidden(step) {
  if (hasErrorSubstep[step.id]) {
    return false;
  } else if (options["hide-same-code"] && step.tags.includes("same-code")) {
    return true;
  } else if (! options.step_change && step.kind == "Change") {
    return true;
  } else if (step.tags.some((tag) => options["hide-" + tag])) {
    return true;
  } else if (options["hide_empty_diff"] && step.diff == "") {
    return true;
  }
  return false;
}

function stepToHTML(step, display) {
  if (!display) {
    return "";
  }
  if (display == 'only_errors' && !hasErrorSubstep[step.id]) {
    return "";
  }
  if (display != 'full' && isStepHidden(step)) {
    return "";
  }

  // console.log("steptohtml " + step.id);
  var s = "";

  // Recursive steps
  var sSubs = "";
  var isStepExpanded = expanded[step.id];
  var substepsDisplay = isStepExpanded || 'only_errors';
  for (var i = 0; i < step.sub.length; i++) {
    var substep = steps[step.sub[i]];
    sSubs += stepToHTML(substep, substepsDisplay)
  }

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
      sTime = nb + "ms";
    }
    var sTimeClass = "exectime-small";
    if (t > 50) {
      sTimeClass = "exectime-heavy";
    } else if (t > 10) {
      sTimeClass = "exectime-mid";
    }
    if (sTime != "") {
      sTime = "<span class='" + sTimeClass + "'>" + sTime + "</span>";
    }
  }

  var sHasMsg = "";
  if (step.debug_msgs && step.debug_msgs.length > 0) {
    sHasMsg = "<span class='has-debug-msg'>MSG</span>";
  }

  var sKind = "";
  if (options.kind) {
    sKind = "[" + escapeHTML(step.kind) + "] ";
  }

  var sScript = escapeHTML(step.script).trim();
  if (step.kind == "Small") {
    // remove the leading '!! ' or '!!!'
    if (sScript.startsWith('!!!')) {
      sScript = sScript.substring(3).trim();
    } else if (sScript.startsWith('!!')) {
      sScript = sScript.substring(2).trim();
    }
  }
  sScript = sScript.replace(/\./,'<wbr>.');

  var sOnClick = "";
  if (step.hasOwnProperty("id")) { // LATER: refine
    sOnClick = "onclick='loadStepDetails(" + step.id + ")'";
  }
  var sTags = "";
  if (options.tags) {
    for (var t = 0; t < step.tags.length; t++) {
      sTags += "<span class='step-tag'>" + step.tags[t] + "</span>";
    }
  }
  var sName = escapeHTML(step.name);
  if (!options.basic_modules) {
    sName = sName.replace('_basic','');
    sName = sName.replace('_loop_list','');
  }
  sName = sName.replaceAll('.','<wbr>.');
  sName = sName.replaceAll('(','(<wbr>');

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

  // Toggle expand symbol
  var sOnClickToggleStep = " onmousedown='expandClick(event, " + step.id + ")'";
  var sStepExpandClass = "step-leaf";
  if (step.sub.some(substep => !isStepHidden(steps[substep]))) {
    sStepExpandClass = "step-expand";
  }
  if (isStepExpanded == "full") {
    sStepExpandClass += " step-fully-expanded";
  } else if (isStepExpanded) {
    sStepExpandClass += " step-expanded";
  }

  // Validity Symbol
  var sValiditySymbol;
  if (root_checking_validity) {
    let sValidityMeaning;
    if (step.isvalid) {
      sValiditySymbol = "&#10004;" // check
      sValidityMeaning = "Validated step";
      lineClass += " step-valid";
    } else if (step.has_valid_parent) {
      sValiditySymbol = "&diams;"; // diamond, validity is covered by a parent
      sValidityMeaning = "Validity covered by a parent";
      lineClass += " step-valid-parent";
    } else if (! step.check_validity) {
      sValidityMeaning = "Validity not checked";
      sValiditySymbol = "&#9679;" // circle, was not trying to check validity
      lineClass += " step-unchecked";
    } else { // step.check_validity && ! step.isvalid
      sValiditySymbol= "&#10060"; // cross, for a trustme step
      sValidityMeaning = "Trusted unverified step";
      lineClass += " step-invalid";
    }
    sValiditySymbol = "<abbr class='step-validity' title='"+ sValidityMeaning +"'>" + sValiditySymbol + "</abbr>"
  } else {
    sValiditySymbol = "";
  }

  // Line color
  if (step.kind == "Big") {
    lineClass += " step-big";
  } else if (step.kind == "Small") {
    lineClass += " step-small";
  } else if (step.kind == "Root") {
    lineClass += " step-root"
  }
  if (step.tags.includes("show")) {
    lineClass += " step-show";
  }
  if (serialized_trace == undefined && step.diff == undefined) {
    lineClass += " step-nodiff";
  }

  s += "<div id='tree-step-" + step.id + "' class='tree-step " + lineClass + "'><span class='"+ sStepExpandClass +"' " + sOnClickToggleStep + "></span><span " + sOnClick + " class='step-title'>" + sTime + sHasMsg + sKind + sName + sScript + sArgs + sTags + sValiditySymbol + "</span></div>"

  if (options.justif) {
    for (var i = 0; i < step.justif.length; i++) {
      s += "<div class='step-justif-text'>" + escapeHTML(step.justif[i]) + "</div>"
    }
  }

  // Substeps
  s += "<ol class='step-sub'> " + sSubs + "</ol>\n";

  if (display == 'outermost') {
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
  if (!options.noop_steps && (step.code_before == step.code_after)) {
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
  // var shouldShowDetails = ($("#treeDiv").html() == "");
  resetView();

  var rootStep = steps[0];
  $("#treeDiv").html(stepToHTML(rootStep, 'outermost'));

  loadStepDetails(selectedStep); // TODO inline here?
}

// handles update after click on "normal" or "full" button
function optionsCheckboxUpdate() {
  // update checkbox display // TODO: use this also in other place
  for (var key in options) {
    var elem = $('#option_' + key);
    if (elem.length == 1) {
      elem.prop('checked', options[key]);
    }
  }
  initControls();
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
  options["view_diff"] = true;
  options["typeinfo_annot"] = true;
  options["basic_modules"] = true;
  options["args"] = true;
  options["exectime"] = true;
  options["step_change"] = true;
  optionsCheckboxUpdate();
  reloadTraceView();
}

function initOptions() {
  for (var i = 0; i < optionsDescr.length; i++) {
    var descr = optionsDescr[i];
    if (descr.radio && !descr.key) {
      descr.key = descr.radio + "_" + descr.value
    }
    optionsDefault[descr.key] = descr.default;
    options[descr.key] = descr.default;
  }
}

function initControls() {
  var sTreeControls = "";
  var sAstControls = "";

  // Generate checkboxes
  for (var i = 0; i < optionsDescr.length; i++) {
    var descr = optionsDescr[i];
    // skip advanced options if not options advanced selected
    if (! options.advanced && (descr.kind == "advanced" || descr.kind == "tag")) {
      continue;
    }
    var id = "option_" + descr.key;
    var oncheck = "updateOptions()";
    if (descr.name == "advanced") {
      oncheck += "; initControls()";
    }
    let sControl;
    if (descr.radio) {
      sControl = htmlRadio(id, descr.name, "ast-radio", descr.radio, oncheck);
    } else {
      sControl = htmlCheckbox(id, descr.name, "ast-checkbox", oncheck);
    }
    if (descr.kind == "ast") {
      sAstControls += sControl;
    } else if (descr.kind == "serialized_ast") {
      if (serialized_trace) {
        sAstControls += sControl;
      }
    } else {
      sTreeControls += sControl;
    }
  }

  // Full/normal button
  sTreeControls += htmlButton("button_normal", "normal", "details-button", "viewDetailsNormal()");
  sTreeControls += htmlButton("button_full", "full", "details-button", "viewDetailsFull()");

  $("#treeControls").html(sTreeControls);
  $("#astControls").html(sAstControls);

  // initialize checkboxes
  for (var i = 0; i < optionsDescr.length; i++) {
    var descr = optionsDescr[i];
    var id = "option_" + descr.key;
    $('#' + id).prop('checked', options[descr.key]);
  }
}

// handles modification of options by click on the checkboxes
function updateOptions() {
  for (var i = 0; i < optionsDescr.length; i++) {
    var descr = optionsDescr[i];
    var elem = $('#option_' + descr.key);
    if (elem.length == 1) {
      options[descr.key] = elem.prop('checked');
    }
  }
  reloadTraceView();
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
      kind: "tag",
      default: val,
    };
    optionsDescr.push(descr);
    options[key] = val;
  }
}

document.addEventListener('DOMContentLoaded', function () {
  var isRootedTrace = (steps[0].kind == "Root");
  root_checking_validity = steps[0].check_validity;
  initEditor();
  initAllTags();
  initOptions();
  initControls();
  initSplitView();
  initTree(0, 0, false); // the root is its own parent, has no valid parent
  // editor.setValue("click on a button");
  if (isRootedTrace) {
  } else {
    $('.row-title').hide();
    $('.row-br').hide();
  }

  // start by showing the tree of steps on the root, or the requested step
  selectedStep = 0; // root
  if (typeof startupOpenStep !== "undefined") {
    // if a step is targeted, expand it
    expanded[startupOpenStep] = true;
  } else {
    // expand big and small steps
    expandRecursively(0, true, 'Small');
  }
  // ensure errors are all visible
  checkErrorSubstep(0);
  // display tree
  reloadTraceView(); // calls loadStepDetails(selectedStep)
});

// alternative:
// but there could be many lines..


//// vertical split resizing

var resizer, leftSplit, rightSplit;

function initSplitView() {
  // TODO: loop over elements with class splitter
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
