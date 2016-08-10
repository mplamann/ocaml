(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t

val zero : t
(** The 63-bit integer 0. *)

val one : t
(** The 63-bit integer 1. *)

val minus_one : t
(** The 63-bit integer -1. *)

external neg : t -> t = "%int63_neg"
(** Unary negation. *)

external add : t -> t -> t = "%int63_add"
(** Addition. *)

external sub : t -> t -> t = "%int63_sub"
(** Subtraction. *)

external mul : t -> t -> t = "%int63_mul"
(** Multiplication. *)

external div : t -> t -> t = "%int63_div"
(** Integer division.  Raise [Division_by_zero] if the second
   argument is zero.  This division rounds the real quotient of
   its arguments towards zero, as specified for {!Pervasives.(/)}. *)

external rem : t -> t -> t = "%int63_mod"
(** Integer remainder.  If [y] is not zero, the result
   of [Int63.rem x y] satisfies the following property:
   [x = Int63.add (Int63.mul (Int63.div x y) y) (Int63.rem x y)].
   If [y = 0], [Int63.rem x y] raises [Division_by_zero]. *)

val succ : t -> t
(** Successor.  [Int63.succ x] is [Int63.add x Int63.one]. *)

val pred : t -> t
(** Predecessor.  [Int63.pred x] is [Int63.sub x Int63.one]. *)

val abs : t -> t
(** Return the absolute value of its argument. *)

val max_int : t
(** The greatest representable 63-bit integer, 2{^62} - 1. *)

val min_int : t
(** The smallest representable 63-bit integer, -2{^62}. *)

external logand : t -> t -> t = "%int63_and"
(** Bitwise logical and. *)

external logor : t -> t -> t = "%int63_or"
(** Bitwise logical or. *)

external logxor : t -> t -> t = "%int63_xor"
(** Bitwise logical exclusive or. *)

val lognot : t -> t
(** Bitwise logical negation *)

external shift_left : t -> int -> t = "%int63_lsl"
(** [Int63.shift_left x y] shifts [x] to the left by [y] bits.
   The result is unspecified if [y < 0] or [y >= 63]. *)

external shift_right : t -> int -> t = "%int63_asr"
(** [Int63.shift_right x y] shifts [x] to the right by [y] bits.
   This is an arithmetic shift: the sign bit of [x] is replicated
   and inserted in the vacated bits.
   The result is unspecified if [y < 0] or [y >= 63]. *)

external shift_right_logical : t -> int -> t = "%int63_lsr"
(** [Int63.shift_right_logical x y] shifts [x] to the right by [y] bits.
   This is a logical shift: zeroes are inserted in the vacated bits
   regardless of the sign of [x].
   The result is unspecified if [y < 0] or [y >= 63]. *)

external of_int : int -> t = "%int63_of_int"
(** Convert the given integer (type [int]) to a 63-bit integer
    (type [int63]). *)

external to_int : t -> int = "%int63_to_int"
(** Convert the given 63-bit integer (type [Int63.t]) to an
    integer (type [int]).  On 64-bit platforms, [int] is 63-bits,
    so no bits are lost. On 32-bit platforms, the 63-bit integer
    is taken modulo 2{^31}, i.e. the top 32 bits are lost
    during the conversion. *)

external of_int32 : int32 -> t = "%int63_of_int32"
(** Convert the given 32-bit integer (type [int32])
   to a 63-bit integer (type [Int63.t]). *)

external to_int32 : t -> int32 = "%int63_to_int32"
(** Convert the given 63-bit integer (type [Int63.t]) to a
   32-bit integer (type [int32]). The 63-bit integer
   is taken modulo 2{^32}, i.e. the top 31 bits are lost
   during the conversion.  *)

external of_int64 : int64 -> t = "%int63_of_int64"
external to_int64 : t -> int64 = "%int63_to_int64"

external of_nativeint : nativeint -> t = "%int63_of_nativeint"
(** Convert the given native integer (type [nativeint])
   to a 63-bit integer (type [Int63.t]). *)

external to_nativeint : t -> nativeint = "%int63_to_nativeint"
(** Convert the given 63-bit integer (type [Int63.t]) to a
   native integer.  On 32-bit platforms, the 63-bit integer
   is taken modulo 2{^32}.  On 64-bit platforms,
   the conversion is exact. *)

external of_string : string -> t = "caml_int63_of_string"
(** Convert the given string to a 63-bit integer.
   The string is read in decimal (by default) or in hexadecimal,
   octal or binary if the string begins with [0x], [0o] or [0b]
   respectively.
   Raise [Failure "int_of_string"] if the given string is not
   a valid representation of an integer, or if the integer represented
   exceeds the range of integers representable in type [Int63.t]. *)

val to_string : t -> string
(** Return the string representation of its argument, in decimal. *)

val compare: t -> t -> int
(** The comparison function for 63-bit integers, with the same specification as
    {!Pervasives.compare}.  Along with the type [t], this function [compare]
    allows the module [Int63] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}. *)

val equal: t -> t -> bool
(** The equal function for int63s. *)
