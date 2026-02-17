(** OxCaml mode, jkind, and modality tests *)

(** {1 Arrow argument modes} *)

val local_arg : string @ local -> int

val unique_arg : string @ unique -> int

val local_unique : string @ local unique -> int

(** {1 Arrow return modes} *)

val ret_mode : string -> int @ local

val multi_mode : string @ local unique -> int @ local

(** {1 No modes} *)

val normal_fun : string -> int

(** {1 Value modalities} *)

val portable_val : int @@ portable

val global_val : string @@ global

(** {1 Type parameter jkinds} *)

type ('a : float64) float_box

type ('a : immediate) imm_box
