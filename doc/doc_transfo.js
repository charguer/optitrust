
var enableWarning = false;

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


// Could use Underscore's _escape method.
function escapeHTML(s) {
   return s.replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;');
}

function loadDiffFromString(targetId, diffString) {
   // this function should be called only after DOM contents is loaded
  var targetElement = document.getElementById(targetId);
  var diff2htmlUi = new Diff2HtmlUI(targetElement, diffString, configuration);
  diff2htmlUi.draw();
  diff2htmlUi.highlightCode();
}

// removes from src everything between the line that contains ".cpp" or "fun" and the line that contains "dump" or ")"
function shrinkSrc(src) {
   src = src.replace("\n\n","\n");
   src = src.replace("  \n\n","\n");
   var start = src.indexOf(".cpp");
   if (start == -1)
      start = src.indexOf("fun _");
   if (start == -1)
      return src;
   start = src.indexOf("\n", start);
   if (start == -1)
      return src;
   src = src.substr(start+1);
   var stop = src.lastIndexOf("dump");
   if (stop == -1) {
      stop = src.lastIndexOf(")");
   }
   if (stop != -1) {
      return src.substr(0,stop);
   }
   return src;
}

function reportError(targetId, targetJsFilename) {
   $('#'+targetId).html("Error loading " + targetJsFilename + ". Check that the '*_doc.js' file exists. If you opening transfo_doc.html, check also that this file is included explicitly in transfo_doc.html.").addClass("error");
   // that you open the chromium browser with flag --disable-web-security, e.g., via 'make opendoc', and that all the *_doc.js files have been generated using 'make doc'.
}

function loadTestFromFileAssumedLoaded(targetId, targetName, sWarning) {
   // function names
   var targetJsFunctionSpec = "get_spec_" + targetId;
   var targetJsFunctionSrc = "get_src_" + targetId;
   var targetJsFunctionDiff = "get_diff_" + targetId;
   // Fill the test block
   var specId = targetId + "_doc";
   var srcId = targetId + "_src";
   var diffId = targetId + "_diff";

   var elem = $('#'+targetId);
   // DEPRECATED var title = elem.html();
   // DEPRECATED var ascombi = elem.is(".ascombi");
   // DEPRECATED var pieces = targetName.split("/");
   // DEPRECATED var kind = pieces[0]; // unused
   // DEPRECATED var name = pieces[1]; // unused
   var contents = "<div class='name'>"+targetName+"</div><div class='spec' id='"+specId+"'></div><div class='src' id='"+srcId+"'></div><div class='diff' id='"+diffId+"'></div>";
   elem.html(contents);

   // Fill the spec part
   var specContents = eval(targetJsFunctionSpec + "()");
   $('#'+specId).html("<pre>"+specContents+"</pre>");

   // Fill the source part
   var srcContents = eval(targetJsFunctionSrc + "()");
   /* DEPRECATED
   if (ascombi) {
     srcContents = srcContents.replace("_basic.", ".");
   } */
   var srcHTML = "<pre><code class='ocaml'>"+escapeHTML(shrinkSrc(srcContents))+"</code></pre>";
   var srcWarning = "";
   if (enableWarning && sWarning) {
      srcWarning = "<div class='warning'>" + sWarning + "</div>";
   }
   $('#'+srcId).html(srcWarning + srcHTML);
   //hljs.highlightElement(srcId);

   // Fill the diff part
   var diffContents = eval(targetJsFunctionDiff + "()");
   loadDiffFromString(diffId, diffContents);
}

function loadTestFromFile(targetId) {
   var targetName = targetId;
   loadTestFromFileAssumedLoaded(targetId, targetName);

   /* DEPRECATED
   var targetName = targetId.replace("__", "/");
   try { // try loading from an include
      loadTestFromFileAssumedLoaded(targetId, targetName);
   } catch (error) {
      var targetJsFilename = "../tests/" + targetName + "_doc.js";
      // The dynamic loading of the JS file requires --disable-web-security
      $.getScript(targetJsFilename, function(data, status) {
         if (status != "success") {
            reportError(targetId, targetJsFilename);
            return;
         }
         loadTestFromFileAssumedLoaded(targetId, targetName, "Warning: JS file loaded the unsafe way.");
      }).fail(function () {
         reportError(targetId, targetName + "_doc.js");
      });
   }*/
}

function loadAllTestFromFile() {
   var all = $(".test").each(function() {
     loadTestFromFile(this.id);
   });
}

document.addEventListener('DOMContentLoaded', function () {
   loadAllTestFromFile();

});


/*
var all = $(".mbox").map(function() {
    return this.innerHTML;
}).get();

$("button").click(function(){
  $.getScript("demo_ajax_script.js");
});
*/
