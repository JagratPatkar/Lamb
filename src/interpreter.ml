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
exception SyntaxError
let eval expr env = 

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
  
  let isNamedFunc expr = 
    match expr with
    | Closure(env,func) -> (match func with 
                           | Fun(S(fn),fp,fb) -> [(fn,expr)]
                           | _ -> [])
    | _ -> []
  in

  let rec pr expr env  = 
      match expr with 
      | Num e -> Num e
      | Var s -> lookup s env
      | Add(exp1,exp2) -> (try 
                            let Num e1 = pr exp1 env in
                            let Num e2 = pr exp2 env in 
                            Num(e1 + e2)
                          with e -> raise IlleagalValue)
      |Ifgreater(exp1,exp2,exp3,exp4) ->  (try 
                                              let Num v1 = pr exp1 env in
                                              let Num v2 = pr exp2 env in 
                                              if v1 > v2
                                              then (pr exp3 env)
                                              else (pr exp4 env)
                                            with e -> raise IlleagalValue)
      | Fun (n,v,e) -> Closure(env,Fun(n,v,e))
      | Let(b,exp,body) -> let e1 = pr exp env in pr body ((b,e1)::env) 
      | Pair(exp1,exp2) -> let e1 = pr exp1 env in 
                            let e2 = pr exp2 env in 
                            Pair(e1,e2)
      | Car e -> (try 
                  let Pair(e1,_) = pr e env in
                  e1
                  with e -> raise IlleagalValue) 
      | Cdr e -> (try 
                  let Pair(_,e2) = pr e env in
                  e2
                  with e -> raise IlleagalValue) 
      | Unit -> Unit
      | Isunit e -> if (pr e env) = Unit 
                  then Num(1)
                  else Num(0)
      | Closure(e,ev) -> Closure(e,ev)
      | Call(clo,arg) -> try 
                          let Closure(e,funb) = (pr clo env) in 
                          let act = (pr arg env) in 
                          let (fn,fp,fb) = getFunParts funb in 
                          if fn = F(false) 
                          then (pr fb ((fp,act)::e))
                          else
                          let S form =  fn in   (pr fb ((form,Closure(e,funb))::((fp,act)::e)))
                          with e ->  raise IlleagalValue
      in 
      let rec iter l exl env=
        if l = []
        then exl
        else 
          let exp = pr (hd l) env in 
            let nextenv = isNamedFunc exp in
            iter (tl l) ( exl @ [exp] ) nextenv    
      in 
      (iter expr [] env);;
  




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

let rec createList l = 
  match l with
  | [] -> []
  | Delim(s)::l' -> if s = " "
                    then createList l'
                    else [s] @ createList l'
  | Text(s)::l' -> [s] @ createList l';;



let rec parser str = 
  let stringHelper x =
    let str = (matched_string x) in
    let rec explode s =
       let rec expl i l = 
        if i < 0 then l else
        expl (i - 1) (s.[i] :: l) in 
      expl (String.length s - 1) []
    in
     let rec implode l = 
       match l with
       | [] -> ""
       | e::l' -> e ^ implode l'
    in
      
      let lis = explode str in 
      let mappedlist = List.map (fun x -> String.make 1 ' ' ^ String.make 1 x ^  String.make 1 ' ') lis in 
      let im = implode mappedlist in 
      im 
      
    in

    let fil1 = Str.global_substitute (Str.regexp("[(-)]+")) stringHelper str in
    let nns = Str.split (Str.regexp "[ \n\t]+")  fil1 in
    let rec looper s =
      let proccessList s =
        let rec processor l ml = 
          let ret = looper l in
          match ret with
          | (Some x,e) -> processor e (ml @ [x])
          | (None,en) -> (ml,en)
        in 
        let rec makeList l = 
            match l with 
            | [] -> Unit 
            | e1::l' -> Pair(e1,(makeList l'))
        in let (pl,e) = processor s [] in 
        let l = makeList pl in 
        (Some(l),e)
      in 
      match s with 
      | e::s' -> 
                 if e = "("
                 then looper s'
                 else if Str.string_match(Str.regexp "[0-9]+$") e 0
                 then (Some(Num(int_of_string e)),s')
                 else if e = "U"
                 then (Some(Unit),s')
                 else if e = "pair"
                 then 
                      try 
                        let (Some v1,e1) = looper s' in
                        let (Some v2,e2) = looper e1 in
                        let (None, e3) = looper e2 in
                        (Some(Pair(v1,v2)),e3)
                      with e-> raise SyntaxError
                 else if e = "list" 
                 then  proccessList s' 
                 else if e = "+"
                 then try 
                        let (Some v1,e1) = looper s' in
                        let (Some v2,e2) = looper e1 in
                        let (None, e3) = looper e2 in
                        (Some(Add(v1,v2)),e3)
                      with e->  raise SyntaxError
                 else if e = "car"
                 then try 
                        let (Some v1,e1) = looper s' in
                        let (None, e2) = looper e1 in
                        (Some(Car(v1)),e2)
                      with e-> raise SyntaxError
                else if e = "cdr"
                then try 
                        let (Some v1,e1) = looper s' in
                        let (None, e2) = looper e1 in
                        (Some(Cdr(v1)),e2)
                      with e-> raise SyntaxError
                else if e = "unit?"
                then try 
                        let (Some v1,e1) = looper s' in
                        let (None, e2) = looper e1 in
                        (Some(Isunit(v1)),e2)
                      with e-> raise SyntaxError
                else if e = "cmp"
                then try 
                          let (Some v1,e1) = looper s' in
                          let (Some v2,e2) = looper e1 in
                          let (Some v3,e3) = looper e2 in
                          let (Some v4,e4) = looper e3 in
                          let (None, e5) = looper e4 in
                          (Some(Ifgreater(v1,v2,v3,v4)),e5)
                        with e-> raise SyntaxError
                else if e = "let"
                then try 
                        let (Some vl,e1) = looper s' in
                        let (Some exp2,e2) = looper e1 in 
                        let (Some exp3,e3) = looper e2 in 
                        let (None, e4) = looper e3 in
                        let Var v = vl in
                        (Some(Let(v,exp2,exp3)),e4)
                      with e-> raise SyntaxError
                else if e = "define"
                then try 
                        let (Some vl,e1) = looper s' in
                        let (Some vl2,e2) = looper e1 in 
                        let (Some exp3,e3) = looper e2 in 
                        let (None, e4) = looper e3 in
                        let Var v = vl in
                        let Var v1 = vl2 in
                        (Some(Fun(S(v),v1,exp3)),e4)
                      with e-> raise SyntaxError
                else if e = "lambda"
                then try 
                        let (Some vl,e1) = looper s' in
                        let (Some exp3,e2) = looper e1 in 
                        let (None, e3) = looper e2 in
                        let Var v = vl in
                        (Some(Fun(F(false),v,exp3)),e3)
                      with e-> raise SyntaxError
                else if e = "call"
                then try 
                        let (Some vl,e1) = looper s' in
                        let (Some vl2,e2) = looper e1 in 
                        let (None, e3) = looper e2 in
                        (Some(Call(vl,vl2)),e3)
                      with e->  raise SyntaxError
                else if Str.string_match(Str.regexp "[A-Za-z_]+$") e 0
                then (Some(Var(e)),s')
                else if e = ")"
                then (None,s')
                else raise SyntaxError
      | _ -> raise SyntaxError
    in
      try 
        let rec m n = 
            let (Some e,en) =  looper n 
            in 
            if en = []
            then [e]
            else  [e] @ (m en) in
        (m nns)
      with e -> raise SyntaxError;;

let  print l = 
   let rec printer exp = 
    match exp with
    | Num(x) -> string_of_int x
    | Unit -> "U"
    | Pair(x,y) -> "(pair " ^ printer x ^ " , " ^ printer y ^ " " ^ ")"
    | Var x -> ""
    | Add(a,b) -> ""
    | Ifgreater(e1,e2,e3,e4) -> ""
    | Fun(e1,e2,e3) -> ""
    | Call(c,f) -> ""
    | Let(e1,e2,e3) -> ""
    | Car(e) -> ""
    | Cdr(e) -> ""
    | Isunit(e) -> ""
    | Closure(e,fp) -> ""
   in 
   let rec m n = 
     match n with 
     | [] -> []
     | e::n' -> [printer e] @ m n' in 
   
  (m l)


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


let rec printer l = 
  match l with 
  | [] -> printf ""
  | e::l' -> printf "%s\n" e; printer_test l'



let () = 
  let _ = eval_tests () in 
  let input_file = (open_in (Sys.argv.(1))) in
  let str = really_input_string input_file (in_channel_length input_file) in 
  (* parser_test str; *)
  printer_test (print (eval (parser str) []))
