open! Core

(** A characterized scale is a scale in which the intervals between degrees have
    all been characterized, that is to say their acoustic natures have been
    specified in details. This allows to distinguish between, for example, just
    and pythagorean major scales. *)

type t = Characterized_interval.t list

val major_just : t
val major_pythagorean : t
val major_just_e53 : t
val major_pythagorean_e53 : t
val major_e12 : t
