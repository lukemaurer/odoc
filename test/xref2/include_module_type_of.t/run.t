Repro of problem from uwt (https://github.com/ocaml/odoc/issues/691)

  $ cat uwt_base.mli
  (* This file is part of uwt, released under the MIT license. See LICENSE.md for
     details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. *)
  module Base : sig
    type 'a uv_result = 'a
  
  module Fs_types : sig
    type uv_open_flag =
      | O_RDONLY (** Open for reading *)
    (** Flags for {!Fs_functions.openfile}
  
        [O_CLOEXEC] doesn't exist, because this flag is unconditionally
        added by libuv. [O_SHARE_DELETE], [O_SHARE_WRITE], [O_SHARE_READ]
        are always added on Windows, unless [O_EXLOCK] is specified. *)
  
  end
  
  module type Fs_functions = sig
    include module type of Fs_types
    with type uv_open_flag = Fs_types.uv_open_flag
  
    type 'a t
  
    val openfile : ?perm:int -> mode:uv_open_flag list -> string -> int t
    (** Equivalent to open(2). perm defaults are 0o644 *)
  end
  end
  
  include module type of Base
    with type Fs_types.uv_open_flag = Base.Fs_types.uv_open_flag
  

What used to happen is that the `odoc link` command would cause an internal
error. If it doesn't here, that particular issue is fixed!

  $ ocamlc -c -bin-annot uwt_base.mli
  $ odoc compile uwt_base.cmti
  File "uwt_base.cmti":
  Warning: Failed to lookup module type (root Uwt_base).Fs_functions Unresolved original module path identifier((root Uwt_base).Base.Fs_types, false) (Lookup failure (module): (root Uwt_base).Base.Fs_types)
  File "uwt_base.cmti":
  Warning: Failed to compile expansion for include : module type of identifier((root Uwt_base).Fs_types, false) with [root.uv_open_flag = identifier((root Uwt_base).Fs_types, false).uv_open_flag] Unresolved original module path identifier((root Uwt_base).Base.Fs_types, false) (Lookup failure (module): (root Uwt_base).Base.Fs_types)
  File "uwt_base.cmti":
  Warning: Failed to lookup module type (root Uwt_base).Fs_functions Unresolved original module path identifier((root Uwt_base).Base.Fs_types, false) (Lookup failure (module): (root Uwt_base).Base.Fs_types)
  File "uwt_base.cmti":
  Warning: Failed to compile expansion for include : module type of identifier((root Uwt_base).Fs_types, false) with [root.uv_open_flag = identifier((root Uwt_base).Fs_types, false).uv_open_flag] Unresolved original module path identifier((root Uwt_base).Base.Fs_types, false) (Lookup failure (module): (root Uwt_base).Base.Fs_types)
  $ odoc link uwt_base.odoc
  File "uwt_base.odoc":
  Warning: While resolving the expansion of include at File "uwt_base.mli", line 28, character 0
  Failed to resolve module type expr module type of r((root Uwt_base).Fs_types) Unresolved original module path identifier((root Uwt_base).Base.Fs_types, false) (Lookup failure (module): (root Uwt_base).Base.Fs_types)

