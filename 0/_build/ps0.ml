(* CS51 Spring 2014
 * PS 0 *)

(* The library we'll be using in this course *)
open Core.Std

(* 1. Please define these variables with the appropriate values.
 * Be sure that these statements all type-check after editing them.
 * You can do this by hitting Ctrl+c and then Ctrl+e in Emacs, or by
 * typing "make" in the terminal emulator *)

(* 1.a. Replace FIRST and LAST with your first and last name *)
let name : (string * string) = ("ANTONIO", "RECALDE");;

(* 1.b. Replace "Other ..." in class_year with your current year that is of
 * type 'year' *)
type year = Freshman | Sophomore | Junior | Senior | Other of string;;

let class_year : year = Other "Extension School Student";;

(* 1.c. Replace the .... with what you're excited about in this course *)
let exciting : string = "I am excited about everything. But I am also intimidated by the fact that OCaml looks so unfamiliar.";;

(* ***
   2. You shouldn't change anything below this line, but you should
   read to the bottom of the file and try to figure out what is going on.
  **** *)

let print = Printf.printf;;

let print_survey () =
  let (first, last) = name in
  let string_year =
    (match class_year with
       | Freshman -> "2017"
       | Sophomore -> "2016"
       | Junior -> "2015"
       | Senior -> "2014"
       | Other s -> "Other: " ^ s
    ) in
    (print "----------------------------------------\n";
     print "Name: %s %s\n\n" first last;
     print "Year: %s\n\n" string_year;
     print "%s\n\n" exciting;
     print "----------------------------------------\n\n";);;


print_survey ();;

(* type "make" to compile the file.
  type ./ps0.native to run the program and print the output.
  Make sure all the values look right.  If they do, submit and
  you're done! *)
