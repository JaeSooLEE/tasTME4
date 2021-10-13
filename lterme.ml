type lambdater =
  |Vari of string
  |Abs of string * lambdater
  |App of lambdater * lambdater


type simplet =
  |Varit of string
  |Fleche of simplet * simplet




let rec pretty_printer_lt lbdt = 
  match lbdt with
  | Vari s ->  s
  | Abs (x, l) -> "λ" ^ x ^ "." ^ (pretty_printer_lt l)
  | App (m, n) -> (pretty_printer_lt m)^" "^(pretty_printer_lt n)

let rec pretty_printer_st st =
  match st with
  | Varit s -> s
  | Fleche (x, y) -> (pretty_printer_st x)^"->"^(pretty_printer_st y);;



print_string (pretty_printer_lt (Abs ("x", Abs("y",App (Vari "x", Vari "y")))))
(*
let rec simplet_egal t1 t2 =
  match (t1, t2) with
  |(Varit a, Varit b) -> a == b
  |(Fleche (a1, a2), Fleche (b1, b2)) -> simplet_egal a1 b1 && 
*)

let rec decompose_fleche t =
  match t with
  |Varit s -> (Varit s)::[]
  |Fleche (a,b) -> (decompose_fleche a)@(decompose_fleche b)
  


let rec simplet_egal_util l1 l2 =
  match (l1, l2) with
  |((Varit a)::[], (Varit b)::[]) -> a = b
  |((Varit a)::at, (Varit b)::bt) -> a = b && simplet_egal_util at bt
  | _ -> false

let rec simplet_egal st1 st2 = simplet_egal_util (decompose_fleche st1) (decompose_fleche st2);;


let stringFromBool value =
  if value then "True" else "False";;

print_string (stringFromBool (simplet_egal (Fleche (Fleche(Varit "a", Varit "b"), Varit "c")) (Fleche(Varit "a", Fleche(Varit "b", Varit "d")))))

