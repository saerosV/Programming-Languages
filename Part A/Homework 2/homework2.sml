(* string string -> boolean
 * Returns true if both strings are the same.
 * Using this function (provided in "hw2provided.sml") will prevent the next
 * functions of having polymorphic types, avoiding confusion. *)

fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* 1-A
 * string * string list -> Option
 * Takes a string and a string list as arguments, returns NONE if the string
 * is not in the list, otherwise returns SOME lst, where lst is identical to
 * the argument list, except it the string is not in it *)

fun all_except_option (str, []) = NONE
  | all_except_option (str, x::xs') = case same_string(str, x) of
                                          true => SOME xs'
                                        | false => case all_except_option (str, xs') of
                                                       NONE => NONE
                                                     | SOME s => SOME(x::s)

(* 1-B
 * string list list * string -> string list
 * Takes a list of list of strings (substitutions) and a string (str)
 * and returns a string list, containing all the strings that are in
 * some list in substitutions that also contains str, except for str itself
 * !!! *)

fun get_substitutions1 (substitutions, str) =
    case substitutions of
        [] => []
      | x::xs' => case all_except_option(str, x) of
                      NONE => get_substitutions1(xs', str)
                    | SOME lst => lst @ get_substitutions1(xs', str)

(* 1-C
 * string list list * string -> string list
 * Tail-recursive version of get_substitutions1 *)

fun get_substitutions2 (substitutions, str) =
    let
        fun aux (substitutions, acc) =
            case substitutions of
                [] => acc
              | x::xs' => case all_except_option(str, x) of
                              NONE => aux(xs', acc)
                            | SOME lst => aux(xs', acc@lst)
    in
        aux(substitutions, [])
    end

(* 1-D
 * string list list * {first:string,middle:string,last:string} ->
 *                    {first:string,middle:string,last:string} list
 * Takes a list of substitutions and a full name, returns a list of full names
 * with all possible you can produce by substituting the first name only. The
 * answer should begin with the original name, followed by 0 or more alternative
 * names. *)

fun similar_names (substitutions, full_name as {first=fst,middle=mdl,last=lst}) =
    let
        fun helper (substitutions, acc) =
            case substitutions of
                [] => acc
              | x::xs' => helper(xs', acc@[{first=x, last=lst, middle=mdl}])
    in
        helper(get_substitutions2 (substitutions,fst), [{first=fst,last=lst,middle=mdl}])
    end

(* Datatype definitions provided for problem 2
 * you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* 2-A
 * card -> color
 * Takes a card and returns its color:
 * Spades and Clubs    => Black
 * Diamonds and Hearts => Red *)

fun card_color (suit, rank) =
    case suit of
        (Clubs | Spades) => Black
      | _      => Red

(* 2-B
 * card -> int
 * Takes a card and returns its value:
 * Numbered cards have their number as the value
 * Aces are 11
 * Everything else is 10 *)

fun card_value (suit, rank) =
    case rank of
        Ace   => 11
      | Num i => i
      | _     => 10

(* 2-C
 * card list * card * exn -> card list
 * Takes a list of cards (cs), a card (c) and an exception (e), returns a list that has all
 * the elements of cs except c. If c is on the list more than once, remove only the first
 * ocurrence. If c is not on the list, raise the exception e. *)

fun remove_card (card_list, card, e) =
    let
        fun helper (card_list, acc) =
            case card_list of
                [] => acc
              | x::xs' => if x = card
                          then acc@xs'
                          else helper(xs', acc@[x])
        val filtered_list = helper(card_list, [])
    in
        case filtered_list = card_list of
            true => raise e
          | false => filtered_list
    end

(* 2-D
 * card list -> Boolean
 * Takes a list of cards and returns true if all cards in the list are of the
 * same color *)

fun all_same_color (card_list) =
    case card_list of
        [] => true
      | _::[] => true
      | x::xs'::ys' => (card_color(x) = card_color(xs') andalso all_same_color(xs'::ys'))

(* 2-E
 * card list -> int
 * Takes a list of cards and returns the sum of all their values *)

fun sum_cards (card_list) =
    let
        fun helper (card_list, acc) =
            case card_list of
                [] => acc
              | x::xs' => helper(xs', card_value(x) + acc)
    in
        helper(card_list, 0)
    end

(* 2-F
 * card list * int -> int
 * Takes a card list and a goal (int) and returns a score.
 * Sum is the sum of the values of held-cards (card_list). If sum is greater than
 * goal, the preliminary score is three times (sum - goal), else it is (goal - sum).
 * The final score is the preliminary score unless all the held-cards are of the
 * same color, in which case the final score is preliminary score divided by 2. *)

fun score (card_list, goal) =
    let
        fun helper (card_list) =
            let
                val sum = sum_cards (card_list)
            in
                case sum > goal of
                    true => 3 * (sum - goal)
                  | false => (goal - sum)
            end
        val preliminary_score = helper(card_list)
    in
        case all_same_color (card_list) of
            true => preliminary_score div 2
          | false => preliminary_score
    end

(* 2-G
 * card list * move list * int -> score
 * Takes a card list (the card-list) a move list (what the player “does” at each
 * point), and an int (the goal) and returns the score at the end of the game
 * after processing (some or all of) the moves in the move list in order.
 *
 * - The game starts with the held-cards being the empty list.
 *
 * - The game ends if there are no more moves. (The player chose to stop since the
 *   move list is empty.)
 *
 * - If the player discards some card c, play continues (i.e., make a recursive call)
 *   with the held-cards not having c and the card-list unchanged. If c is not in the
 *   held-cards, raise the IllegalMove exception.
 *
 * - If the player draws and the card-list is (already) empty, the game is over. Else
 *   if drawing causes the sum of the held-cards to exceed the goal, the game is over
 *  (after drawing). Else play continues with a larger held-cards and a smaller card-list. *)

fun officiate (card_list, move_list, goal) =
    let
        fun helper (card_list, move_list, held_cards) =
            case move_list of
                [] => score(held_cards, goal)
              | Discard(c)::ys' => helper(card_list, ys', remove_card(held_cards, c, IllegalMove))
              | Draw::ys' => case card_list of
                                 [] => score(held_cards, goal)
                               | x::xs' => case sum_cards(held_cards@[x]) > goal of
                                               true => score(held_cards@[x], goal)
                                             | false => helper(remove_card(card_list, x, IllegalMove),
                                                               ys',
                                                               held_cards@[x])
    in
        helper(card_list, move_list, [])
    end
