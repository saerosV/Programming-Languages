(* Homework3 Tests*)

use "homework3.sml";

val only_capitals_test0 = only_capitals [] = []
val only_capitals_test1 = only_capitals ["bow","tie","bro"] = []
val only_capitals_test2 = only_capitals ["Yellow","blue","brown"] = ["Yellow"]
val only_capitals_test3 = only_capitals ["Not","a","String","Sir"] = ["Not","String","Sir"]

val longest_string1_test0 = longest_string1 [] = ""
val longest_string1_test1 = longest_string1 ["a","b","c"] = "a"
val longest_string1_test2 = longest_string1 ["yes","no","maybe"] = "maybe"
val longest_string1_test3 = longest_string1 ["certainly","probably"] = "certainly"

val longest_string2_test0 = longest_string2 [] = ""
val longest_string2_test1 = longest_string2 ["a","b","c"] = "c"
val longest_string2_test2 = longest_string2 ["yes","no","maybe"] = "maybe"
val longest_string2_test3 = longest_string2 ["certainly","probably"] = "certainly"

val longest_string3_test0 = longest_string3 [] = ""
val longest_string3_test1 = longest_string3 ["a","b","c"] = "a"
val longest_string3_test2 = longest_string3 ["yes","no","maybe"] = "maybe"
val longest_string3_test3 = longest_string3 ["certainly","probably"] = "certainly"

val longest_string4_test0 = longest_string4 [] = ""
val longest_string4_test1 = longest_string4 ["a","b","c"] = "c"
val longest_string4_test2 = longest_string4 ["yes","no","maybe"] = "maybe"
val longest_string4_test3 = longest_string4 ["certainly","probably"] = "certainly"

val longest_capitalized_test0 = longest_capitalized [] = ""
val longest_capitalized_test1 = longest_capitalized ["a","b"] = ""
val longest_capitalized_test2 = longest_capitalized ["aa","Ab","Ac","ad"] = "Ab"
val longest_capitalized_test3 = longest_capitalized ["Lol","no","Broski"] = "Broski"

val rev_string_test0 = rev_string "" = ""
val rev_string_test1 = rev_string "Alu" = "ulA"
val rev_string_test2 = rev_string "pdf" = "fdp"
val rev_string_test3 = rev_string "commendable" = "elbadnemmoc"


val first_answer_test0 = first_answer (fn x => if x > 3 then SOME x else NONE)
                                      [1,2,3,4,5]
                         = 4
val first_answer_test1 = (first_answer (fn x => if x > 3 then SOME x else NONE)
                                       [1,2,3] ; false)
                         handle NoAnswer => true;
                         
val all_answers_test0 = all_answers (fn x => if x < 1 then SOME [x] else NONE)
                                    []
                        = SOME []
val all_answers_test1 = all_answers (fn x => if x = 1 then SOME [x] else NONE)
                                    [2,3,4,5,6,7]
                        = NONE
val all_answers_test2 = all_answers (fn x => if x > 1 then SOME [x] else NONE)
                                    [2,3,4,5,6,7]
                        = SOME [2,3,4,5,6,7]
val all_answers_test3 = all_answers (fn x => if x mod 2 = 0 then SOME [x] else NONE)
                                    [2,4,5,6,8]
                        = NONE
val all_answers_test4 = all_answers (fn x => if x mod 2 = 0 then SOME [x] else NONE)
                                    [2,4,6,8]
                        = SOME [2,4,6,8]


val count_wildcards_test0 = count_wildcards Wildcard = 1
val count_wildcards_test1 = count_wildcards (Variable "str") = 0
val count_wildcards_test2 = count_wildcards (TupleP [Wildcard, ConstP 12, Wildcard]) = 2
val count_wildcards_test3 = count_wildcards (ConstructorP("pattern", (TupleP [Wildcard, ConstP 12, Wildcard]))) = 2

val count_wild_and_variable_lengths_test0 =
    count_wild_and_variable_lengths (Variable("a")) = 1
val count_wild_and_variable_lengths_test1 =
    count_wild_and_variable_lengths Wildcard = 1
val count_wild_and_variable_lengths_test2 =
    count_wild_and_variable_lengths (TupleP [Wildcard, Variable "str", Wildcard]) = 5
val count_wild_and_variable_lengths_test3 =
    count_wild_and_variable_lengths (ConstructorP
                                         ("pattern",
                                          (TupleP [Wildcard, ConstP 4]))) = 1
                                    
val count_some_var_test0 =
    count_some_var ("x", Variable("y")) = 0
val count_some_var_test1 =
    count_some_var ("x", Variable("x")) = 1
val count_some_var_test2 =
    count_some_var ("x", (TupleP [Wildcard, Variable("x"), Variable("n")])) = 1
val count_some_var_test3 =
    count_some_var ("z", (TupleP [Variable("n"), Variable("z"), Variable("z")])) = 2
    
val check_pat_test0 =
    check_pat (Variable("x")) = true
val check_pat_test1 =
    check_pat (TupleP [Wildcard, Variable "x", Wildcard]) = true
val check_pat_test2 =
    check_pat (TupleP [Wildcard, Variable "x", Variable "y"]) = true
val check_pat_test3 =
    check_pat (TupleP [Wildcard, Variable "x", Variable "x"]) = false
val check_pat_test4 =
    check_pat (ConstructorP("x", (TupleP [Wildcard, Variable "x", Wildcard]))) = true
val check_pat_test5 =
    check_pat (ConstructorP("x", (TupleP [Wildcard, Variable "x", ConstructorP("y", Variable "y")]))) = true
val check_pat_test6 =
    check_pat (ConstructorP("x", (TupleP [Wildcard, Variable "x", ConstructorP("y", Variable "x")]))) = false
val check_pat_test7 =
    check_pat (ConstructorP("x", (TupleP [Wildcard, Variable "x", ConstructorP("y", TupleP [Variable "y"])]))) = true
val check_pat_test8 =
    check_pat (ConstructorP("x", (TupleP [Wildcard, Variable "x", ConstructorP("y", TupleP [Variable "z"])]))) = true
val check_pat_test9 =
    check_pat (ConstructorP("x", (TupleP [Wildcard, Variable "x", ConstructorP("y", TupleP [Variable "x"])]))) = false
val check_pat_test10 =
    check_pat (ConstructorP("x", (ConstructorP("y", TupleP [Variable "x", Variable "y"])))) = true
val check_pat_test11 =
    check_pat (ConstructorP("x", (ConstructorP("y", TupleP [Variable "x", Variable "x"])))) = false
val check_pat_test12 =
    check_pat (TupleP [Wildcard, Variable "x", TupleP [Variable "y"]]) = true


val match_test0 = match (Const(1), UnitP) = NONE
val match_test1 = match (Const(1), ConstP 1) = SOME []
val match_test2 = match (Const(1), Variable "s") = SOME [("s", Const(1))]
val match_test3 = match (Const(1), TupleP [Wildcard]) = NONE
val match_test4 = match (Tuple [Unit], TupleP [UnitP]) = SOME []
val match_test5 = match (Tuple  [Tuple [Unit]],
                      TupleP [TupleP[UnitP, Variable "x"]]) = NONE
val match_test6 = match (Tuple  [Const (1), Tuple [Unit]],
                      TupleP [ConstP 1,  TupleP[UnitP]]) = SOME []
val match_test9 = match (Tuple  [Const (1), Tuple [Unit, Const(2)]],
                      TupleP [ConstP 1,  TupleP[UnitP, Variable("s")]]) = SOME [("s", Const(2))]
val match_test10 = match (Tuple  [Const (1), Tuple [Unit, Const(2)]],
                      TupleP [ConstP 2,  TupleP[UnitP, Variable("s")]]) = NONE
val match_test11 = match (Tuple  [Const (1), Tuple [Unit, Const(2)]],
                      TupleP [ConstP 1,  TupleP[UnitP, Variable("s"), Wildcard]]) = NONE
                      

val first_match_test0 = first_match Unit [UnitP] = SOME []

val first_match_test1 = first_match Unit [Variable ("s")] = SOME [("s", Unit)]

val first_match_test2 = first_match (Tuple   [Const(1), Tuple [Unit, Const(2)]])
                           [(TupleP [ConstP 1, TupleP[UnitP, Variable("s")]])] = SOME [("s", Const(2))]

val first_match_test3 = first_match (Tuple [Const(1), Tuple [Unit, Const(2)]])
                           [(TupleP [ConstP 1, TupleP[UnitP, ConstP 3]])] = NONE                      
