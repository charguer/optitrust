
var contents = window.atob("");
// put in the string the result of
//  cat foo.json | base64 -w 0

contents[0] =`

{ "foo": "bar",
   "val": 0
}
`;

source[1] = {};

source[0] =`
 ...
`;

source[1] =`
 ...
`;

/*

variant of dump_trace  -> dump_trace_to_js

   that opens juste one single file  -> out

   output_string out_enc "contents[" + i + "] = `\n";
   ast_json_to_doc out
   output_string out_enc "`;\n\n";


   same with   source[i]
   ast_to_doc out


   then for each file



jsdiff package
*/
