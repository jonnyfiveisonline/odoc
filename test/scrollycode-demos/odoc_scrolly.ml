(* Custom odoc binary with the scrollycode extension statically linked.

   The scrollycode extension registers itself when this module is loaded,
   via the [let () = ...] at the bottom of scrollycode_extension.ml.

   We force it to be linked by referencing it, then invoke the standard
   odoc CLI entry point. *)

(* Force-link the extension module *)
let () =
  ignore (Scrollycode_extension.Scrolly.prefix : string)

(* Include the full odoc CLI - this is main.ml without the dune-site loading *)
include Odoc_scrolly_main
