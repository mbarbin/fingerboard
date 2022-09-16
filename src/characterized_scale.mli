open! Core

(** A characterized scale is a scale in which the interval between
   degrees have all been characterized, that is their acoustic nature
   has been determined in details. This allows to distinguish between,
   for example, just and pythagorean major scales. *)

type t = Characterized_interval.t list
