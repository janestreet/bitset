open! Core

(** [Bitset] provides a space-efficient set of non-negative integer values. *)

module type S = sig
  type -'perms t

  (** [create ~len] creates a bitset with at least the given initial length (in bits). The
      set is initially empty, and does not grow. *)
  val create : len:int -> [< read_write ] t

  val create_local : len:int -> [< read_write ] t
  val capacity : [> read ] t -> int

  (** [is_empty t] returns [true] iff [mem t i] returns false on every [i] *)
  val is_empty : [> read ] t -> bool

  (** [add t i] adds [i] to the set. *)
  val add : [> write ] t -> int -> unit

  val unsafe_add : [> write ] t -> int -> unit

  (** [remove t i] removes [i] from set. *)
  val remove : [> write ] t -> int -> unit

  val unsafe_remove : [> write ] t -> int -> unit

  (** [assign t i x = if x then add t i else remove t i], but branch-free. *)
  val assign : [> write ] t -> int -> bool -> unit

  val unsafe_assign : [> write ] t -> int -> bool -> unit

  (** [mem t i] returns [true] iff [i] is in [t]. *)
  val mem : [> read ] t -> int -> bool

  val unsafe_mem : [> read ] t -> int -> bool

  (** [clear t] empties the set. *)
  val clear : [> write ] t -> unit

  (** [set_all t] adds everything to the set. *)
  val set_all : [> write ] t -> unit

  (** [union a b] combines two bitsets; it creates a new bitset where [i] is present iff
      [i] is in [a] or [b] *)
  val union : [> read ] t -> [> read ] t -> [< read_write ] t

  val union_local : [> read ] t -> [> read ] t -> [< read_write ] t

  (** [union_into ~dst ~src] adds all elements in [src] into [dst]. All elements set in
      [src] must be below [capacity dst]. This function raises if that is not the case. *)
  val union_into : dst:[> write ] t -> src:[> read ] t -> unit

  (** [inter a b] intersects two bitsets; it creates a new bitset where [i] is present iff
      [i] is in [a] and [b] *)
  val inter : [> read ] t -> [> read ] t -> [< read_write ] t

  val inter_local : [> read ] t -> [> read ] t -> [< read_write ] t
  val inter_into : dst:[> write ] t -> src:[> read ] t -> unit

  (** [is_inter_empty a b = is_empty (inter_local a b)], but skips the intermediate
      allocation.

      This function is optimized for small bitsets and minimizes branching.  For large
      bitsets, note that the entire intersection is computed (no early exit for non-zero
      intersection).  *)
  val is_inter_empty : [> read ] t -> [> read ] t -> bool

  (** [diff a b] finds the elements in one but not the other bitset; it creates a new
      bitset where [i] is present iff [i] is in [a] and not [b] *)
  val diff : [> read ] t -> [> read ] t -> [< read_write ] t

  val diff_local : [> read ] t -> [> read ] t -> [< read_write ] t

  (** [remove_all ~dst ~src] removes all elements set in [src] from [dst]. It is the
      in-place version of [diff] *)
  val remove_all : dst:[> write ] t -> src:[> read ] t -> unit

  (** [complement t] creates a new bitset where [i] is present iff [i] is not in [t]. This
      operates only on [i]s from 0 to [capacity t] (which may be larger than the initial
      [len]) *)
  val complement : [> read ] t -> [< read_write ] t

  val complement_local : [> read ] t -> [< read_write ] t
  val complement_inplace : [> read_write ] t -> unit

  (** returns the number of members -- values of [i] for which [mem t i] is true *)
  val num_members : [> read ] t -> int

  (** returns the number of members -- values of [i] for which [mem t i] is true --
      within the range defined by start and end

      Raises if start is outside of [0, capacity t) or end is outside of [0, capacity t].
  *)
  val num_members_in_range
    :  [> read ] t
    -> start:int Maybe_bound.t
    -> end_:int Maybe_bound.t
    -> int

  (** returns the index of the first element set in [t] *)
  val first_member : [> read ] t -> int option

  (** [iter t ~f] calls [f] for all elements in set [t] *)
  val iter_set : [> read ] t -> f:(int -> unit) -> unit

  (** [grow t ~new_len] creates a new set from [t] with capacity [new_len]. *)
  val grow : [> read ] t -> new_len:int -> [< read_write ] t

  val grow_local : [> read ] t -> new_len:int -> [< read_write ] t

  (** [copy_and_truncate t ~new_len] creates a new set from [t] with capacity [new_len].
      Bits above [new_len] may be cleared. *)
  val copy_and_truncate : [> read ] t -> new_len:int -> [< read_write ] t

  val copy_and_truncate_local : [> read ] t -> new_len:int -> [< read_write ] t
  val copy : [> read ] t -> [< read_write ] t
  val copy_local : [> read ] t -> [< read_write ] t

  (** Converts [t] into a string with length [capacity t], where each char is either '0'
      for an unset bit or '1' for a set bit. *)
  val to_string : [> read ] t -> string

  (** Like [to_string], but returns a local [string]. *)
  val to_string_local : [> read ] t -> string

  (** Opposite of [to_string]. *)
  val of_string : string -> [< read_write ] t

  (** Like [of_string], but returns a local [t]. *)
  val of_string_local : string -> [< read_write ] t

  val sexp_of_t : [> read ] t -> Sexp.t
  val t_of_sexp : Sexp.t -> [< read_write ] t
  val quickcheck_generator : [< read_write ] t Quickcheck.Generator.t
end

module type S_plain = sig
  type t [@@deriving equal]

  include S with type 'perms t := t
end

module type S_permissioned = sig
  type -'perms t [@@deriving equal]

  include S with type 'perms t := 'perms t
end
