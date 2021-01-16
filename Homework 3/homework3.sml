(* 1
 * string list -> string list
 * Takes a string list and returns a string list containing only the string in
 * the argument that start with an uppercase letter.
 * ASSUME: All string have at least 1 (one) character *)

fun only_capitals xs =
    List.filter(fn s => Char.isUpper(String.sub (s,0))) xs

(* Another approach I find a little more readable than the previous version.

fun only_capitals xs =
    let
        fun isFirstCharacterUpper s = Char.isUpper(String.sub(s,0))
    in
        List.filter(isFirstCharacterUpper) xs
    end
 *)

(* 2
 * string list -> string
 * Takes a string list and returns the longest string in the list. If the list
 * is empty, return "". In the case of a tie, return the string closest to the
 * beginning of the list. *)

(* foldl: fn : (’a * ’b -> ’b) -> ’b -> ’a list -> ’b *)

fun longest_string1 str_lst =
    foldl(fn (x,acc) => if String.size(x) > String.size(acc) then x else acc)
         ""
         str_lst

(* 3
 * string list -> string
 * Exactly like longest_string1, except it returns the string closest to the end
 * of the list, in case of a tie *)

fun longest_string2 str_lst =
    foldl(fn (x,acc) => if String.size(x) < String.size(acc) then acc else x)
         ""
         str_lst

(* 4a
 * (int * int -> bool) -> string list -> string
 * If longest_string_helper is passed a function that behaves like > (so it
 * returns true exactly when its first argument is stricly greater than its
 * second), then the function returned has the same behavior as longest_string1.
 *)

fun longest_string_helper f str_lst =
    foldl(fn (x,acc) => if f(String.size(x),String.size(acc)) then x else acc)
         ""
         str_lst

(* 4b
 * string list -> string
 * Has the same behavior as longest_string1 *)

val longest_string3 = longest_string_helper (fn (x,y) => x > y)

(* 4c
 * string list -> string
 * Has the same behavior as longest_string2 *)

val longest_string4 = longest_string_helper (fn (x,y) => x >= y)


(* 5
 * string list -> string
 * Takes a string list and returns the longest string in the list that begins
 * with an uppercase letter, or "" if there are no such strings. In the case
 * of a tie, return the string closest to the beginning of the list.
 * ASSUME: all strings have at least 1 character. *)

val longest_capitalized = longest_string1 o only_capitals

(* 6
 * string -> string
 * Takes a string and returns the string that has the same characters in reverse
 * order. *)

val rev_string = String.implode o List.rev o String.explode

(* 7
 * (’a -> ’b option) -> ’a list -> ’b
 * The first argument should be applied to elements of the second argument in
 * order until the first time it returns SOME v for some v and then v is the
 * result of the call to first_answer. If the first argument returns NONE for
 * all list elements, then first_answer should raise the exception NoAnswer. *)

exception NoAnswer

fun first_answer f lst =
    case lst of
        [] => raise NoAnswer
      | x::xs' => case f(x) of
                      SOME v => v
                    | NONE => first_answer f xs'

(* 8
 * (’a -> ’b list option) -> ’a list -> ’b list option
 * The first argument should be applied to elements of the second argument.
 * If it returns NONE for ANY element, then the result for all_answers is NONE.
 * Else the calls to the first argument will have produced SOME lst1, ... SOME
 * lstn and the result of all_answers is SOME lst where lst is lst1, lst2, ...,
 * lstn appended together (order doesn’t matter).
 * Note: all_answers f [] should evaluate to SOME [] *)

fun all_answers f lst =
    let
        fun helper (lst, acc) =
            case lst of
                [] => SOME acc
              | x::xs' => case f(x) of
                              SOME lstn => helper(xs', acc@lstn)
                            | _ => NONE
    in
        helper (lst, [])
    end

(* Provided datatypes and function for problems 9-12*)

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let
	val r = g f1 f2
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(* 9a
 * pattern -> int
 * Takes a pattern and returns how many Wildcards it contains *)

fun count_wildcards p =
    g (fn x => 1) (fn s => 0) p

(* 9b
 * pattern -> int
 * Takes a pattern and returns the number of Wildcard patterns it contains plus
 * the sum of the string lengths of all the variables in the variable patterns
 * it contains. *)

fun count_wild_and_variable_lengths p =
    g (fn x => 1) (fn s => String.size s) p

(* 9c
 * string * pattern -> int
 * Takes a string and a pattern (as a pair) and returns the number of times the
 * string appears as a variable in the pattern. *)

fun count_some_var (str, p) =
    g (fn x => 0) (fn y => if str = y then 1 else 0) p

(* 10
 * pattern -> bool
 * Takes a pattern and returns true if and only if all the variables appearing
 * in the pattern are distinct from each other (i.e., use different strings). *)

fun check_pat p =
    let
        fun make_list p =
            case p of
                Variable x        => [x]
              | TupleP ps         => List.foldl(fn (p,x) => (make_list p) @ x) [] ps
              | ConstructorP(_,p) => make_list p
              | _                 => []
        fun all_diferent lst =
            case lst of
                []         => true
              | _::[]      => true
              | x::xs::xs' => x <> xs andalso all_diferent xs'
    in
        all_diferent (make_list p)
    end

(* 11
 * valu * pattern -> (string * valu) list option
 * Takes a valu * pattern and returns a (string * valu) list option, namely NONE
 * if the pattern does not match and SOME lst where lst is the list of bindings
 * if it does. Note that if the value matches but the pattern has no patterns of
 * the form Variable s, then the result is SOME []. *)

fun match (v, p) =
    case (v, p) of
        (_, Wildcard)   => SOME []
      | (v, Variable s) => SOME [(s,v)]
      | (Unit, UnitP)   => SOME []
      | (Const i, ConstP j)  => if i = j then SOME [] else NONE
      | (Tuple vs, TupleP ps) => if List.length(vs) = List.length(ps)
                                 then all_answers match (ListPair.zip (vs,ps))
                                 else NONE
      | (Constructor(s2,v), ConstructorP(s1,p)) => if s1 = s2
                                                   then match (v,p)
                                                   else NONE
      | _ => NONE


(* 12
 * valu -> pattern list -> (string * valu) list option
 * Takes a value and a list of patterns and returns a (string * valu) list option,
 * namely NONE if no pattern in the list matches or SOME lst where lst is the list
 * of bindings for the first pattern in the list that matches.*)

fun first_match v ps =
    SOME (first_answer (fn p => match(v,p)) ps)
    handle NoAnswer => NONE
