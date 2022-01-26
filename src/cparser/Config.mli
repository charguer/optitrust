
val arch: string
  (** Target architecture *)

val model: string
  (** Sub-model for this architecture *)

val abi: string
  (** ABI to use *)

val is_big_endian: bool
  (** Endianness to use *)

val system: string
  (** Flavor of operating system that runs CompCert *)

val stdlib_path: string
  (** Path to CompCert's library *)

val has_runtime_lib: bool
  (** True if CompCert's library is available. *)

val has_standard_headers: bool
  (** True if CompCert's standard header files is available. *)

type response_file_style =
  | Gnu         (* responsefiles in gnu compatible syntax *)
  | Diab        (* responsefiles in diab compatible syntax *)
  | Unsupported (* responsefiles are not supported *)

val response_file_style: response_file_style
  (** Style of supported responsefiles *)

val gnu_toolchain: bool
  (** Does the targeted system use the gnu toolchain *)
