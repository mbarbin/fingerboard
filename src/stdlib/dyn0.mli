(*_********************************************************************************)
(*_  Fingerboard-stdlib - Extending OCaml's Stdlib for Fingerboard                *)
(*_  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: MIT OR AGPL-3.0-or-later                            *)
(*_********************************************************************************)

(** {1 Including dune's Dyn module}

    This module is designed to shadow dune's [Dyn] module. As such it re-exports
    its original interface. *)

include module type of struct
  include Dyn
end

(** {1 Builder}

    This extends the existing interface to build dyn values with helpers that
    we've found convenient while working with this abstraction. *)

val inline_record : string -> (string * Dyn.t) list -> Dyn.t
