
let arch = "x86"
let model = "64"
let abi = "standard"
let is_big_endian = false (* "little endian" *)
let system = "linux"
let has_runtime_lib = true
let has_standard_headers = true
let stdlib_path = "/usr/local/lib/compcert"
type response_file_style =
  | Gnu         (* responsefiles in gnu compatible syntax *)
  | Diab        (* responsefiles in diab compatible syntax *)
  | Unsupported (* responsefiles are not supported *)
let response_file_style = Gnu
let gnu_toolchain = true
