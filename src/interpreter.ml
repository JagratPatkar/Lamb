open Printf;;
open Str;;
open List;;

type name = 
  | F of bool
  | S of string

type expr = 
| Num of int
| Var of string
| Add of expr * expr
| Ifgreater of expr * expr * expr * expr 
| Fun of name * string * expr 
| Call of expr * expr
| Let of string * expr * expr
| Pair of expr * expr
| Car of expr
| Cdr of expr
| Unit
| Isunit of expr
| Closure of env * expr
and env = (string * expr) list


exception IlleagalValue

let rec eval expr env = 

  let rec lookup str env = 
    match env with
    | (s,v)::env' -> if str = s 
                     then v
                     else (lookup str env')
    | [] -> failwith (sprintf "unboud variable during evaluation %s" str) in 
  
  let getFunParts expr =
    match expr with
    | Fun(fn,fp,fb) -> (fn,fp,fb)
    | _ -> raise IlleagalValue in

  match expr with 
  | Num e -> Num e
  | Var s -> lookup s env
  | Add(exp1,exp2) -> (try 
                        let Num e1 = eval exp1 env in
                        let Num e2 = eval exp2 env in 
                        Num(e1 + e2)
                      with e -> raise IlleagalValue)
  |Ifgreater(exp1,exp2,exp3,exp4) ->  (try 
                                          let Num v1 = eval exp1 env in
                                          let Num v2 = eval exp2 env in 
                                          if v1 > v2
                                          then (eval exp3 env)
                                          else (eval exp4 env)
                                        with e -> raise IlleagalValue)
  | Fun (n,v,e) -> Closure(env,Fun(n,v,e))
  | Let(b,exp,body) -> let e1 = eval exp env in eval body ((b,e1)::env) 
  | Pair(exp1,exp2) -> let e1 = eval exp1 env in 
                        let e2 = eval exp2 env in 
                        Pair(e1,e2)
  | Car e -> (try 
              let Pair(e1,_) = eval e env in
              e1
              with e -> raise IlleagalValue) 
  | Cdr e -> (try 
              let Pair(_,e2) = eval e env in
              e2
              with e -> raise IlleagalValue) 
  | Unit -> Unit
  | Isunit e -> if (eval e env) = Unit 
              then Num(1)
              else Num(0)
  | Closure(e,ev) -> Closure(e,ev)
  | Call(clo,arg) -> try 
                      let Closure(e,funb) = (eval clo env) in 
                      let act = (eval arg env) in 
                      let (fn,fp,fb) = getFunParts funb in 
                      if fn = F(false) 
                      then (eval fb ((fp,act)::e))
                      else
                      let S form = fn in (eval fb ((form,Closure(e,funb))::((fp,act)::e)))
                      with e -> raise IlleagalValue
  




let eval_tests () = 
  let t1 = (eval (Num(10)) []) = Num(10) in
  let t2 = (eval (Var("x")) [("x",Num(10))]) = Num(10) in 
  (*let t3 = (eval (Var("x")) [("y",Num(10))]) = Num(10);;*) (* Passed *)
  let t4 = (eval (Add(Var("x"),Num(10))) [("x",Num(10))]) = Num(20) in 
  let t5 = (eval (Add(Num(20),Num(10))) [("x",Num(10))]) = Num(30) in 
  (*let t6 = (eval (Add(Num(20),Unit)) [("x",Num(10))]) = Num(30);;*) (* Passed *)
  let t7 = (eval (Ifgreater(Num(10),Num(20),Var("x"),Var("y"))) [("x",Num(20)) ; ("y",Unit)]) = Unit in 
  let t8 = (eval (Ifgreater(Num(100),Num(20),Var("x"),Var("y"))) [("x",Num(20)) ; ("y",Unit)]) = Num(20) in 
  (* let t9 = (eval (Ifgreater(Num(100),Var("y"),Var("x"),Var("y"))) [("x",Num(20)) ; ("y",Unit)]) = Num(20)*) (* Passed *)

  let t10 = (eval (Fun(F(false),"f",Add(Var("f"),Num(10)))) []) = Closure([],(Fun(F(false),"f",Add(Var("f"),Num(10))))) in 
  let t11 = (eval (Fun(F(false),"f",Add(Var("f"),Num(10)))) [("x",Num(10))]) = Closure([("x",Num(10))],(Fun(F(false),"f",Add(Var("f"),Num(10))))) in

  let t12 = (eval (Let("x",Add(Num(10),Var("x")),Var("x"))) [("x",Num(10))]) = Num(20) in
  let t13 = (eval (Let("x",Let("y",Num(10),Add(Var("y"),Var("z"))),Var("x"))) [("z",Num(30))]) = Num(40) in

  let t14 = (eval (Car(Pair(Num(10),Fun(F(false),"f",Add(Var("f"),Num(10)))))) []) = Num(10) in
  (*let t15 = (eval (Car(Num(10))) [])*) (* Passed *)

  let t16 = (eval (Cdr(Pair(Num(10),Fun(F(false),"f",Add(Var("f"),Num(10)))))) []) = Closure([],Fun(F(false),"f",Add(Var("f"),Num(10)))) in
  (*let t17 = (eval (Cdr(Num(10))) [])*) (* Passed *)

  let t18 = (eval (Isunit(Add(Num(10),Num(20)))) []) = Num(0) in
  let t19 = (eval (Isunit(Let("x",Unit,Var("x")))) []) = Num(1) in

  let t20 = (eval (Closure([],Fun(F(false),"f",Add(Var("f"),Num(10))))) []) = Closure([],Fun(F(false),"f",Add(Var("f"),Num(10)))) in


  let t21 = (eval (Call(Closure([],Fun(F(false),"f",Add(Var("f"),Num(10)))),Var("x"))) [("x",Num(100))]) = Num(110) in
  let t22 = (eval (Call(Closure([],Fun(S("myfunc"),"x",Ifgreater(Isunit(Var("x")),Num(0),Var("x"),Call(Var("myfunc"),Cdr(Var("x")))))),Pair(Num(10),Pair(Num(20), Pair(Num(30),Unit))))) [("x",Num(100))]) = Unit in

  (*printf "Num Test ---> %b \n" t1;
  printf "Var Test ---> %b \n" t2 ;
  (*printf "Var Test ---> %b \n" t3 ;;*)
  printf "Add Test---> %b \n" t4 ;
  printf "Add Test---> %b \n" t5;
  (*printf "Add Test---> %b \n" t6 ;;*)
  printf "Ifgreater Test---> %b \n" t7 ;
  printf "Ifgreater Test---> %b \n" t8 ;
  (* printf "Ifgreater Test---> %b \n" t9 ;; *)
  printf "Fun Test ---> %b \n" t10 ;
  printf "Fun Test ---> %b \n" t11 ;
  printf "Let Test ---> %b \n" t12 ;
  printf "Let Test ---> %b \n" t13 ;
  printf "Car Test ---> %b \n" t14 ;
  (*printf "Car Test ---> %b \n" t15 ;;*)

  printf "Cdr Test ---> %b \n" t16 ;
  (*printf "Cdr Test ---> %b \n" t17 ;;*)

  printf "Isunit Test ---> %b \n" t18 ;
  printf "Isunit Test ---> %b \n" t19 ;

  printf "Closure Test ---> %b \n" t20 ;
  printf "Call Test ---> %b \n" t21 ;
  printf "Recursive Call Test ---> %b \n" t22 ;*)

  if (t1 && t2 && t4 && t5 && t7 && t8  && t10 && t11 && t12 && t13 && t14 && t16 && t18 && t19 && t20 && t21 && t22)
  then printf "Heath Status ===== OK"
  else printf "Helth Status ===== NOT OK";;



let () = 
  let k = eval_tests () in 
  let input_file = (open_in (Sys.argv.(1))) in
  let str = really_input_string input_file (in_channel_length input_file) in 
  