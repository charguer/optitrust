

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


function escapeHTML(s) {
   return s.replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;');
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

// TODO: for inserting code excerpt
// var srcHTML = "<pre><code class='ocaml'>"+escapeHTML(shrinkSrc(srcContents))+"</code></pre>";


// Load the diff for a given target
function loadDiff(targetId) {
  // this function should be called only after DOM contents is loaded
  var diffString = window.atob($("#" + targetId).html());
  var targetElement = document.getElementById(targetId);
  var diff2htmlUi = new Diff2HtmlUI(targetElement, diffString, configuration);
  diff2htmlUi.draw();
  diff2htmlUi.highlightCode();
}

document.addEventListener('DOMContentLoaded', function () {
   var all = $(".diff-unit-test").each(function() {
     loadDiff(this.id);
   });
});
