#use "interpreter.ml"

let eval_tests () = 
  let t1 = (eval ([Num(10)]) []) = [Num(10)] in
  let t2 = (eval ([Var("x")]) [("x",Num(10))]) = [Num(10)] in 
  (*let t3 = (eval (Var("x")) [("y",Num(10))]) = Num(10);;*) (* Passed *)
  let t4 = (eval ([Add(Var("x"),Num(10))]) [("x",Num(10))]) = [Num(20)] in 
  let t5 = (eval ([Add(Num(20),Num(10))]) [("x",Num(10))]) = [Num(30)] in 
  (*let t6 = (eval (Add(Num(20),Unit)) [("x",Num(10))]) = Num(30);;*) (* Passed *)
  let t7 = (eval ([Ifgreater(Num(10),Num(20),Var("x"),Var("y"))]) [("x",Num(20)) ; ("y",Unit)]) = [Unit] in 
  let t8 = (eval ([Ifgreater(Num(100),Num(20),Var("x"),Var("y"))]) [("x",Num(20)) ; ("y",Unit)]) = [Num(20)] in 
  (* let t9 = (eval (Ifgreater(Num(100),Var("y"),Var("x"),Var("y"))) [("x",Num(20)) ; ("y",Unit)]) = Num(20)*) (* Passed *)

  let t10 = (eval ([Fun(F(false),"f",Add(Var("f"),Num(10)))]) []) = [Closure([],(Fun(F(false),"f",Add(Var("f"),Num(10)))))] in 
  let t11 = (eval ([Fun(F(false),"f",Add(Var("f"),Num(10)))]) [("x",Num(10))]) = [Closure([("x",Num(10))],(Fun(F(false),"f",Add(Var("f"),Num(10)))))] in

  let t12 = (eval ([Let("x",Add(Num(10),Var("x")),Var("x"))]) [("x",Num(10))]) = [Num(20)] in
  let t13 = (eval ([Let("x",Let("y",Num(10),Add(Var("y"),Var("z"))),Var("x"))]) [("z",Num(30))]) = [Num(40)] in

  let t14 = (eval ([Car(Pair(Num(10),Fun(F(false),"f",Add(Var("f"),Num(10)))))]) []) = [Num(10)] in
  (*let t15 = (eval (Car(Num(10))) [])*) (* Passed *)

  let t16 = (eval ([Cdr(Pair(Num(10),Fun(F(false),"f",Add(Var("f"),Num(10)))))]) []) = [Closure([],Fun(F(false),"f",Add(Var("f"),Num(10))))] in
  (*let t17 = (eval (Cdr(Num(10))) [])*) (* Passed *)

  let t18 = (eval ([Isunit(Add(Num(10),Num(20)))]) []) = [Num(0)] in
  let t19 = (eval ([Isunit(Let("x",Unit,Var("x")))]) []) = [Num(1)] in

  let t20 = (eval ([Closure([],Fun(F(false),"f",Add(Var("f"),Num(10))))]) []) = [Closure([],Fun(F(false),"f",Add(Var("f"),Num(10))))] in


  let t21 = (eval ([Call(Closure([],Fun(F(false),"f",Add(Var("f"),Num(10)))),Var("x"))]) [("x",Num(100))]) = [Num(110)] in
  let t22 = (eval ([Call(Closure([],Fun(S("myfunc"),"x",Ifgreater(Isunit(Var("x")),Num(0),Var("x"),Call(Var("myfunc"),Cdr(Var("x")))))),Pair(Num(10),Pair(Num(20), Pair(Num(30),Unit))))]) [("x",Num(100))]) = [Unit] in

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
  then printf "Interpreter Heath Status ===== OK \n"
  else printf "Interpreter Helth Status ===== NOT OK \n";;


  let parser_test str = 
    (*let num_test = (eval (parser str) []) = Num(10) in*) (* Passed *)
    (* let unit_test = (eval (parser str) []) = Unit in *) (* Passed *)
    (*let pair_test = (eval (parser str) []) = Pair(Num(10),Pair(Num(20),Unit)) in *)
    (* let list_test1 = (eval (parser str) []) = Pair(Num(10),Pair(Num(20),Unit)) in *)
    (* let add_test = parser str = Add(Add(Num(20),Num(30)),Add(Num(40),Num(50))) in *)
    (* let car_test = (eval (parser str) []) = Num(10) in *)
    (* let cdr_test = (eval (parser str) []) = Pair(Num(20),Pair(Num(30),Pair(Num(40),Pair(Num(50),Unit)))) in *)
    (* let is_unit = (eval (parser str) []) = Num(0) in  *)
    (* let if_greater_test = (eval (parser str) []) = Pair(Num(10),Pair(Num(20),Pair(Num(30),Pair(Num(40),Pair(Num(50),Unit))))) in *)
    (* let let_test = (eval (parser str) []) = Num(40) in *)
    (* let func_test = (eval (parser str) []) = Closure([],Fun(S("fuc"),"x",Add(Num(10),Var("x")))) in *)
    (* let map_test = (eval (parser str) []) = Pair(Num(20),Pair(Num(30),Pair(Num(40),Pair(Num(50),Unit)))) in *)
    let test_data = parser str in
    (* let mult_map_test1 = (eval test_data []) =  [ Pair(Num(20),Pair(Num(30),Pair(Num(40),Pair(Num(50),Unit)))) ; Pair(Num(20),Pair(Num(30),Pair(Num(40),Pair(Num(50),Unit))))] in  *)
    let print_test1 = (eval test_data []) = [Num(10)] in 
    if print_test1
    then printf "Interpretation Successfull \n"
    else printf "Interpretation Unsuccessfull \n"