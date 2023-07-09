let%expect_test "_" =
  print_endline "asdf";
  [%expect {| |}]
;;

open Scheme_interpret

include struct
  open OCanren
  open Gterm

  let app a b = seq Std.(a % !<b)

  (* let list xs =
    match xs with
    | [] -> assert false
    | xs -> seq (Std.List.cons (symb "list") (Std.list Fun.id xs))
  ;; *)

  type iterm = Gterm.injected

  let lambda1 : iterm -> iterm -> iterm =
   fun v b ->
    let open OCanren.Std in
    seq (Std.list Fun.id [ symb !!"lambda"; seq !<v; b ])
 ;;

  let list xs = seq (Std.List.cons (symb !!"list") xs)
  let ( % ) = Std.( % )
  let ( !< ) = Std.( !< )

  let ex1 : iterm -> iterm =
   fun tmp ->
    let lhs = lambda1 tmp (list (tmp % !<(list (symb !!"quote" % !<tmp)))) in
    seq (lhs % !<lhs)
 ;;
end

let%expect_test "_" =
  OCanren.(run q)
    (fun rez ->
      let open OCanren in
      fresh tmp (evalo (ex1 tmp) nil (Gresult.val_ rez)))
    (fun q -> q#reify Gterm.reify)
  |> OCanren.Stream.take ~n:1
  |> List.iteri (fun n t -> Format.printf "%3d: %a\n%!" n (GT.fmt Gterm.logic) t);
  [%expect {| |}]
;;
