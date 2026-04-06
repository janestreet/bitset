@@ portable

(** Bitset implementation backed by a standard OCaml string. *)

module type S_plain = Bitset_intf.S_plain
module type S_permissioned = Bitset_intf.S_permissioned

include S_plain
module Permissioned : S_permissioned

module Expert : sig
  val unsafe_to_bytes : local_ t -> local_ Bytes.t
  val unsafe_of_bytes : local_ Bytes.t -> local_ t
end
