(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "homework1.sml";

val is_older_test1 = is_older ((2014, 4, 7), (2013, 4, 2)) = false
val is_older_test2 = is_older ((2013, 4, 2), (2014, 4, 7)) = true
val is_older_test3 = is_older ((2014, 4, 2), (2014, 3, 2)) = false
val is_older_test4 = is_older ((2014, 3, 2), (2014, 4, 2)) = true
val is_older_test5 = is_older ((2014, 4, 7), (2014, 4, 2)) = false
val is_older_test6 = is_older ((2014, 4, 2), (2014, 4, 7)) = true
val is_older_test7 = is_older ((2014, 4, 2), (2014, 4, 2)) = false

val number_in_month_test1 = number_in_month ([(4, 6, 3), (4, 11, 3)], 2) = 0
val number_in_month_test2 = number_in_month ([(14, 12, 3), (7, 3, 8)], 12) = 1
val number_in_month_test3 = number_in_month ([(44, 5, 3), (4, 5, 3), (3, 5, 7)], 5) = 3

val number_in_months_test1 = number_in_months ([(21, 3, 9), (12, 11, 30)], [4, 7]) = 0
val number_in_months_test2 = number_in_months ([(21, 3, 9), (12, 11, 30)], [3, 2]) = 1
val number_in_months_test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

val dates_in_month_test1 = dates_in_month ([(12, 11, 11), (53, 4, 21)], 3) = []
val dates_in_month_test2 = dates_in_month ([(13, 11, 11), (24, 7, 21)], 7) = [(24, 7, 21)]
val dates_in_month_test3 = dates_in_month ([(22, 1, 12), (33, 1, 2)], 1) = [(22, 1, 12), (33, 1, 2)]

val dates_in_months_test1 = dates_in_months ([(43, 10, 21), (17, 2, 11)], [3, 5]) = []
val dates_in_months_test2 = dates_in_months ([(43, 10, 21), (17, 2, 11)], [2]) = [(17, 2, 11)]
val dates_in_months_test3 = dates_in_months ([(32, 5, 6), (7, 3, 1), (40, 6, 9)], [3, 5]) = [(7, 3, 1), (32, 5, 6)]
val dates_in_months_test4 = dates_in_months ([(43, 2, 21), (17, 8, 11), (28, 6, 30), (34, 11, 14)], [2, 3, 5, 6]) = [(43, 2, 21), (28, 6, 30)]
                             
val get_nth_test1 = get_nth ([], 1) = "Empty list, try again"
val get_nth_test2 = get_nth (["Foo", "bar"], 1) = "Foo"
val get_nth_test3 = get_nth (["This", "is", "a", "test"], 4) = "test" 

val date_to_string_test1 = date_to_string (2010, 12, 15) = "December 15, 2010"
val date_to_string_test2 = date_to_string (1969, 3, 29) = "March 29, 1969"
val date_to_string_test3 = date_to_string (1897, 8, 9) = "August 9, 1897"

val number_before_reaching_sum_test1 = number_before_reaching_sum (10, [1, 2, 3, 4, 5]) = 3
val number_before_reaching_sum_test2 = number_before_reaching_sum (15, [1, 4, 9, 10]) = 3
val number_before_reaching_sum_test3 = number_before_reaching_sum (19, [1, 2, 3, 5, 6, 7]) = 5

val what_month_test1 = what_month 20 = 1
val what_month_test2 = what_month 70 = 3
val what_month_test3 = what_month 121 = 5

val month_range_test1 = month_range (40, 38) = []
val month_range_test2 = month_range (31, 34) = [1, 2, 2, 2]
val month_range_test3 = month_range (149, 152) = [5, 5, 5, 6]

val oldest_test1 = oldest [] = NONE
val oldest_test2 = oldest ([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
val oldest_test3 = oldest ([(1978, 3, 30), (1979, 4, 29), (1979, 3, 30)]) = SOME (1978, 3, 30)
val oldest_test4 = oldest [(5, 5, 2), (5, 10, 2), (5, 2, 2), (5, 12, 2)] = SOME (5, 2, 2)
val oldest_test5 = oldest [(5, 5, 2), (5, 10, 2), (5, 2, 2), (5, 12, 2)] = SOME (5, 2, 2)
