open Scheme_interpret

[@@@ocaml.warnerror "-26-32-33"]

open Gterm
open OCanren

let ( !< ) = Std.( !< )

let expr0 =
  lambda
    (symb !!"x")
    (list [ symb !!"x"; list [ app (symb !!"quote") (symb !!"quote"); symb !!"x" ] ])
;;

let expr1 =
  app
    (lambda
       (symb !!"x")
       (list [ symb !!"x"; list [ app (symb !!"quote") (symb !!"quote"); symb !!"x" ] ]))
    (quote
       (lambda
          (symb !!"x")
          (list
             [ symb !!"x"; list [ app (symb !!"quote") (symb !!"quote"); symb !!"x" ] ])))
;;

let expr2 = lambda (symb !!"x") (symb !!"x")

let get1answer e =
  clear_unifications ();
  OCanren.(run q) (fun x -> evalo e nil x) (fun r -> r#reify Gresult.reify)
  |> OCanren.Stream.take ~n:1
  |> List.iteri (fun i e -> Printf.printf "%d: %s\n\n" i (Gresult.show_logic e));
  Printf.printf "unifications: %d\n" config.unifications
;;

let () = get1answer expr2
let () = get1answer expr1
