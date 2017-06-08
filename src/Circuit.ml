(** an integer set *)
module Set = Set.Make
    (struct
      type t = int
      let compare x y =
        if x = y then 0 else if x < y then -1 else 1
    end)

(** circuit representation *)
type value = Hi | Lo

type t = IN of int (** each input is labelled by an integer *)
       | OUT of t
       | NOT of t
       | AND  of t * t
       | XOR  of t * t
       | OR   of t * t
       | NOR  of t * t
       | NAND of t * t

let rec inputs_of (c : t) : Set.t =
  match c with
      IN i -> Set.add i Set.empty
    | OUT g -> inputs_of g
    | NOT g -> inputs_of g
    | AND (g1,g2) -> Set.union (inputs_of g1) (inputs_of g2)
    | XOR (g1,g2) -> Set.union (inputs_of g1) (inputs_of g2)
    | OR  (g1,g2) -> Set.union (inputs_of g1) (inputs_of g2)
    | NOR (g1,g2) -> Set.union (inputs_of g1) (inputs_of g2)
    | NAND (g1,g2) -> Set.union (inputs_of g1) (inputs_of g2)

let number_of_inputs c = Set.cardinal (inputs_of c)

(* let rec evaluate (g : t) : value = *)
(*   match g with *)
(*     IN v -> v *)
(*   | OUT g -> evaluate g *)
(*   | NOT  g -> negate (evaluate g) *)
(*   | AND (g1,g2) -> conj (evaluate g1) (evaluate g2) *)
(*   | OR  (g1,g2) -> disj (evaluate g1) (evaluate g2) *)

(* let andg i1 i2 = AND (IN i1, IN i2) *)
(* let org i1 i2  = OR (IN i1, IN i2) *)
(* let nandg i1 i2 = NOT (AND (IN i1, IN i2)) *)
(* let norg  i1 i2 = NOT (OR (IN i2, IN i2)) *)
(* let xorg  i1 i2 = AND ((nandg i1 i2), (OR (IN i2, IN i1))) *)
