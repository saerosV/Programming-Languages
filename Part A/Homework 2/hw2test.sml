use "homework2.sml";

(* 1-A *)
val all_except_option_test0 = all_except_option ("red", []) = NONE
val all_except_option_test1 = all_except_option ("square", ["circle", "triangle"]) = NONE
val all_except_option_test2 = all_except_option ("", ["d","r","dn",""]) = SOME ["d","r","dn"]
val all_except_option_test3 = all_except_option ("blue", ["red","yellow","blue","brown", "green"])
                              = SOME["red", "yellow", "brown", "green"]

(* 1-B *)
val get_substitutions1_test0 = get_substitutions1 ([[]], "zed") = []
val get_substitutions1_test1 = get_substitutions1 ([["2","3"], ["4", "5"]], "1") = []
val get_substitutions1_test2 = get_substitutions1 ([["1","3","2"],["4","6","5"],
                                                   ["9","3"]], "3") = ["1","2","9"]
val get_substitutions1_test3 = get_substitutions1 ([["B","E","C","D"],["F","D","Z","C"],
                                                    ["N","L","S"],["C","E"]], "C")
                               = ["B","E","D","F","D","Z","E"]

(* 1-C *)
val get_substitutions2_test0 = get_substitutions2 ([[]], "zed") = []
val get_substitutions2_test1 = get_substitutions2 ([["2","3"], ["4", "5"]], "1") = []
val get_substitutions2_test2 = get_substitutions2 ([["1","3","2"],["4","6","5"],
                                                   ["9","3"]], "3") = ["1","2","9"]
val get_substitutions2_test3 = get_substitutions2 ([["B","E","C","D"],["F","D","Z","C"],
                                                    ["N","L","S"],["C","E"]], "C")
                               = ["B","E","D","F","D","Z","E"]

(* 1-D *)
val similar_names_test0 = similar_names([[]],{first="Joe",middle="Doe",last="Snow"}) =
                          [{first="Joe",middle="Doe",last="Snow"}]

val similar_names_test1 = similar_names([["Will","William"],["Bob","Martin"],["William","Will.a.im"]],
                                        {first="William",middle="Henry",last="Harrison"})
                          = [{first="William",  last="Harrison", middle="Henry"},
                             {first="Will",     last="Harrison", middle="Henry"},
                             {first="Will.a.im",last="Harrison", middle="Henry"}]
val similar_names_test2 = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
                                        {first="Fred", middle="W", last="Smith"})
                          = [{first="Fred",    last="Smith", middle="W"},
                             {first="Fredrick",last="Smith", middle="W"},
                             {first="Freddie", last="Smith", middle="W"},
                             {first="F",       last="Smith", middle="W"}]

(* 2-A *)
val card_color_test0 = card_color(Spades,   Num 3) = Black
val card_color_test1 = card_color(Clubs,     Jack) = Black
val card_color_test2 = card_color(Diamonds, Queen) = Red
val card_color_test3 = card_color(Hearts,  Num 10) = Red

(* 2-B *)
val card_value_test0 = card_value(Clubs,   Num 7) = 7
val card_value_test1 = card_value(Spades,  Num 3) = 3
val card_value_test2 = card_value(Hearts,    Ace) = 11
val card_value_test3 = card_value(Diamonds, Jack) = 10
val card_value_test4 = card_value(Clubs,    King) = 10

(* 2-C *)
val remove_card_test0 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val remove_card_test1 = ((remove_card ([(Hearts, Num 2)], (Hearts, Ace), IllegalMove);
                          false)
                         handle IllegalMove => true)
val remove_card_test2 = remove_card ([(Clubs, Jack), (Diamonds, Queen)],
                                     (Clubs, Jack), IllegalMove) = [(Diamonds, Queen)]
val remove_card_test3 = remove_card ([(Spades, Num 10),(Diamonds, King),(Spades, Num 10)],
                                     (Spades, Num 10), IllegalMove)
                        = [(Diamonds, King),(Spades, Num 10)]
(* 2-D *)
val all_same_color_test0 = all_same_color ([]) = true
val all_same_color_test1 = all_same_color [(Spades, Ace), (Hearts, Ace)] = false
val all_same_color_test2 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val all_same_color_test3 = all_same_color [(Hearts, Ace),(Diamonds, Ace),(Clubs, Queen)] = false
val all_same_color_test4 = all_same_color [(Clubs, King),(Spades, Ace),(Clubs, Queen)] = true

(* 2-E *)
val sum_cards_test0 = sum_cards [] = 0
val sum_cards_test1 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4
val sum_cards_test2 = sum_cards [(Diamonds, Ace), (Spades, Num 7), (Clubs, Queen)] = 28

(* 2-F *)
val score_test0 = score ([(Clubs, Ace),(Hearts, Num 2)], 10)  = 3 * (13 - 10)
val score_test1 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = (10 - 6)
val score_test2 = score ([(Spades, Ace),(Spades, Num 7)], 10) = (3 * (18 - 10)) div 2
val score_test3 = score ([(Hearts, Num 1),(Hearts, Num 3)],10) = (10 - 4) div 2

(* 2-G *)
val officiate_test0 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val officiate_test1 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val officiate_test2 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false)
              handle IllegalMove => true)
