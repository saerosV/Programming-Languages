(* For exercise purposes:
 * Date is year/month/day *)


(* (int * int * int) * (int * int * int) -> bool
 *
 * Evaluates to true if the first argument is a
 * date that comes before the second argument.
 * If both arguments are the same, return false. *)

fun is_older (date1 : (int * int * int), date2 : (int * int * int)) =
    (#1 date1) * 365 + (#2 date1) * 12 + (#3 date1) <
    (#1 date2) * 365 + (#2 date2) * 12 + (#3 date2)

(* (int * int * int) list * int -> int
 * Returns how many dates in a list are in the given month *)

fun number_in_month (date_list : (int * int * int) list, month : int) =
    let fun counter (date_list: (int * int * int) list, acc : int) =
            if null date_list
            then acc
            else if #2 (hd date_list) = month
            then counter (tl date_list, acc + 1)
            else counter (tl date_list, acc)
    in
        counter (date_list, 0)
    end

(* (int * int * int) list * int list -> int
 * Returns how many dates in a list are in the given list of months *)

fun number_in_months (date_list : (int * int * int) list, month_list : int list) =
    if null month_list
    then 0
    else number_in_month  (date_list, hd month_list) +
         number_in_months (date_list, tl month_list)

(* (int * int * int) list * int -> (int * int * int) list
 * Returns a list holding the dates from the argument list of dates that
 * are in the given month *)

fun dates_in_month (date_list : (int * int  * int) list, month : int) =
    if null date_list
    then []
    else if #2 (hd date_list) = month
    then (hd date_list) :: dates_in_month(tl date_list, month)
    else dates_in_month(tl date_list, month)

(* (int * int * int) list * int list -> (int * int * int) list
 * Returns a list holding the dates from the argument list of dates, that
 * are in any of the given months of the second list. *)

fun dates_in_months (date_list : (int * int * int) list, month_list : int list) =
    if null month_list
    then []
    else dates_in_month  (date_list, hd month_list) @
         dates_in_months (date_list, tl month_list)

(* string list * int -> string
 * Takes a list of strings and an int N, returns the Nth element of the list.
 * If the given list is empty, returns a simple "error" message *)

fun get_nth (string_list : string list, nth_element : int) =
    if null string_list
    then "Empty list, try again"
    else
        let fun counter (string_list : string list, acc : int) =
                if acc = nth_element
                then (hd string_list)
                else counter (tl string_list, acc + 1)
         in
             counter (string_list, 1)
         end

(* (int * int * int) -> string
 * Takes a date and returns a string of the form "Month Day, Year",
 * as in "January 20, 2013" *)

fun date_to_string (date : (int * int * int)) =
    let
        val MONTHS = ["January", "February", "March", "April", "May",
                      "June", "July", "August", "September", "October",
                      "November", "December"]
    in
        get_nth (MONTHS, #2 (date))
        ^ " "
        ^ (Int.toString (#3 date))
        ^ ", "
        ^ (Int.toString (#1 date))
    end

(* int * int list -> int
 * Takes an int (sum) and a list of ints, returns an int n such that the
 * first n elements of the list add to less than the sum, but the first
 * n + 1 elements of the list should add to sum or more.
 * Assume that sum and all the elements of the list are positive *)

fun number_before_reaching_sum (sum : int, numbers : int list) =
    let
        fun counter (acc : int, n : int, numbers : int list) =
            if acc + hd (numbers) >= sum
            then n
            else counter (acc + hd (numbers), n + 1, tl (numbers))
    in
        counter (0, 0, numbers)
    end

(* int -> int
 * Takes a day of the year (int between 1 and 365) and returns what month that
 * day is in (3 for March, 4 for April, etc. *)

fun what_month (day : int) =
    let val MONTHS = [31, 28, 31, 30, 31, 30,
                      31, 31, 30, 31, 30, 31]
    in
       number_before_reaching_sum (day, MONTHS) + 1
    end

(* int * int -> int list
 * Takes two days of the year and returns an int list with the months of the
 * days in the range of day1 and day2.
 * After reviewing my solution, I realized the accumulator was unecessary and
 * inefficient, but that's still what I could come up with.  *)

fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else
        let fun counter (acc : int) =
            if acc = day2
            then what_month acc :: []
            else what_month acc :: counter (acc + 1)
        in
            counter (day1)
        end

(* (int * int * int) list -> (int * int * int)
 * Takes a list of dates and evaluates to (int * int * int) option.
 * Produce 'SOME date' if the date is the oldest, else NONE if the
 * list is empty. *)

fun oldest (date : (int * int * int) list) =
    if null date
    then NONE
    else
        let fun counter (date : (int * int * int) list, oldest_date : (int * int * int)) =
            if null (tl date)
            then SOME oldest_date
            else if is_older (oldest_date, hd (tl date))
            then counter (tl date, oldest_date)
            else counter (tl date, hd (tl date))
        in
            counter (date, hd date)
        end
