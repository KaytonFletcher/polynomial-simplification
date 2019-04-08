(* Sum type to encode efficiently polynomial expressions *)
type pExp =
  | Term of int*int (*
      First int is the constant
      Second int is the power of x 
      10  -> Term(10,0)
      2x -> Term(2,1)
      3x^20 -> Term(3, 20)
    *)
  | Plus of pExp list
  (*
    List of terms added
    Plus([Term(2,1); Term(1,0)])
  *)
  | Times of pExp list (* List of terms multiplied *)


(*
  Function to traslate betwen AST expressions
  to pExp expressions
*)
let rec from_expr (_e: Expr.expr) : pExp =
    match _e with 
    | Pos(e) -> from_expr e
    | Neg(e) -> evalNeg (from_expr e)
    | Pow(e, n) -> evalPow (from_expr e) n
        
    | Mul(e1, e2) -> Times([from_expr e1 ; from_expr e2])
    | Add(e1, e2) -> Plus([from_expr e1 ; from_expr e2])
    | Sub(e1, e2) -> Plus([from_expr e1 ; from_expr (Neg(e2))])
    | Num(n) -> Term(n,0)
    | Var(c) -> Term(1,1)

and evalNeg (_p: pExp) : pExp =
      match _p with 
                      (* Turns first term in times negative *)
        | Times(li) -> Times ( (evalNeg (List.hd li)) :: (List.tl li) )
        | Plus(li) -> 
          begin
            let rec negTerms (oldList: pExp list) (newList: pExp list) : (pExp list) =
              match oldList with
              | [] -> newList (* Base cae, list is empty *)
            
              (* Distributing negative to all elems of plus *)
              | _ -> negTerms (List.tl oldList) ( evalNeg (List.hd oldList) :: newList )
            in
            Plus (negTerms li [])
          end
        | Term(n1, n2) -> Term(-1*n1, n2)

and evalPow (exp: pExp) (pow: int): pExp =
begin 
  match exp with
  | Term(n1, n2) -> 
    begin
      match n1 with
      | 0 -> Term( int_of_float ( float_of_int n1  ** float_of_int pow ), 0)
      | _ -> Term(n1, n2*pow)
    end
  | Plus(li) -> 
    begin
      let rec timesPlus (_li: pExp list) (numExpr: int): pExp list =  
          if( numExpr = pow) then _li 
          else timesPlus (Plus(li) :: _li) (numExpr + 1)      
      in Times(timesPlus [] 0) 
    end
  | Times(li) -> 
    begin
      let rec timesEach (oldli: pExp list) (newli: pExp list) : pExp list= 
        match oldli with
        | [] -> newli
        | hd::tl -> timesEach (tl) (( evalPow hd pow ) :: newli ) 
      in Times( timesEach li [])
    end
end    


(* 
  Compute degree of a polynomial expression.

  Hint 1: Degree of Term(n,m) is m
  Hint 2: Degree of Plus[...] is the max of the degree of args
  Hint 3: Degree of Times[...] is the sum of the degree of args 
*)
let rec degree (_e:pExp): int =  (* TODO *)
  match _e with
  | Term(n1, n2) -> n2
  | Times(li) -> sumDegrees li 
  | Plus(li) -> maxDegree li

and sumDegrees (li:pExp list): int =
  match li with
  | [] -> 0
  | hd::tl -> sumDegrees tl + degree hd
   
and maxDegree (li:pExp list) : int =
    degree (List.hd (List.sort compare li)) 

(* 
  Comparison function useful for sorting of Plus[..] args 
  to "normalize them". This way, terms that need to be reduced
  show up one after another.
  *)
and compare (e1: pExp) (e2: pExp) : int =
  let n1 = degree e1 in
  let n2 = degree e2 in
  if n1 > n2 then -1
  else if n1 < n2 then 1
  else 0
  
(* Print a pExpr nicely 
  Term(3,0) -> 3
  Term(5,1) -> 5x 
  Term(4,2) -> 4x^2
  Plus... -> () + () 
  Times ... -> ()() .. ()

  Hint 1: Print () around elements that are not Term() 
  Hint 2: Recurse on the elements of Plus[..] or Times[..]
*)

(* Plus( Times( Term(-2, 0), Term(1, 1) ), Times ( Term(7, 0), Term(1, 1) ) ) *)
let rec print_pExp (_e: pExp): unit =
  (* TODO *)
  ( match _e with
  | Times(li) -> let rec timesPrint (l: pExp list) : unit  =
                match l with
                | [] -> ()
                | _ ->  print_pExp (List.hd l) ; timesPrint (List.tl l)
                in
                timesPrint li  
  | Plus(li) -> let rec plusPrint (l: pExp list) : unit  =
                match l with
                | [] -> ()
                | tl :: [] -> print_pExp tl
                | firstExp :: secExp :: newList -> print_pExp (firstExp) ; 
                  begin
                    match secExp with
                    | Term(n1, n2) -> 
                      begin
                        if n1 >= 0 then ( print_string "+" ; plusPrint (secExp :: newList) )
                        else plusPrint (secExp :: newList)
                      end
                    | _ ->  print_string "+" ; plusPrint ( secExp :: newList)
                  end
                in
                plusPrint li              
  | Term(n1, n2) -> 
    match n2 with
    | 0 -> print_int n1
    | 1 -> print_int n1 ; print_char 'x'
    | p -> print_int n1 ; print_string "x^" ; print_int p
  ) 

(* 
  Function to simplify (one pass) pExpr

  n1 x^m1 * n2 x^m2 -> n1*n2 x^(m1+m2)
  Term(n1,m1)*Term(n2,m2) -> Term(n1*n2,m1+m2)

  Hint 1: Keep terms in Plus[...] sorted
  Hint 2: flatten plus, i.e. Plus[ Plus[..], ..] => Plus[..]
  Hint 3: flatten times, i.e. times of times is times
  Hint 4: Accumulate terms. Term(n1,m)+Term(n2,m) => Term(n1+n2,m)
          Term(n1, m1)*Term(n2,m2) => Term(n1*n2, m1+m2)
  Hint 5: Use distributivity, i.e. Times[Plus[..],] => Plus[Times[..],]
    i.e. Times[Plus[Term(1,1); Term(2,2)]; Term(3,3)] 
      => Plus[Times[Term(1,1); Term(3,3)]; Times[Term(2,2); Term(3,3)]]
      => Plus[Term(2,3); Term(6,5)]
  Hint 6: Find other situations that can arise
*)
let rec simplify1 (e:pExp): pExp =
  begin
    match e with
    | Times(li) -> simplifyTimes li
    | Plus(li) -> simplifyPlus li
    | _ -> e

  end     

and simplifyTimes (li:pExp list): pExp = 
 match li with
 | Term(n1, n2) :: Term(n3, n4) :: [] ->  print_string "Times with exactly two terms. n1: " ; print_int n1 ;  print_string "\n" ;  Term(n1*n3, n2+n4)
 | Term(n1, n2) :: Term(n3, n4) :: tl -> print_string "Times with at least two terms. n1 and n3 " ; print_int n1 ; print_int n3 ; print_string "\n" ; Times(Term(n1*n3, n2+n4) :: tl)
 | hd :: [] -> print_string "Only one thing in times\n" ; hd
 | Term(n1,n2) :: Plus(list) :: tl | Plus(list) :: Term(n1,n2) :: tl -> simplifyTimes (Plus(distTerm list n1 n2) :: tl)
 (* CASE HERE FOR PLUS PLUS FOIL *)
 | Plus(li1) :: Plus(li2) :: tl -> Times(Plus(distribute (flattenPlus li1) (flattenPlus li2)) :: tl)
 | _ -> Times(flattenTimes li)

   (* TAKES IN PLUS LIST AND TERM n1 AND n2 -> returns list with term distributed throughout plus *)
and distTerm (li: pExp list) (n1: int) (n2: int): pExp list = print_string "distTerm\n" ;
match li with
| [] -> print_string "list being distributed is empty\n" ; []
| hd :: tl -> 
  match hd with 
  | Term(n3, n4) ->  print_string "Term -> Term -> Term\n" ; Term(n1*n3, n2+n4) :: (distTerm tl n1 n2)
  | Times(tlist) -> print_string "Times in plus distTerm\n" ; ((distTerm [(getTerm tlist)] n1 n2)) @ (distTerm tl n1 n2)
  | Plus(plist) -> print_string "Plus in plus, not good\n" ; (distTerm plist n1 n2) @ (distTerm tl n1 n2)

and distribute (li1: pExp list) (li2: pExp list): pExp list =
  match li1 with
  | [] -> []
  | hd :: tl -> 
    match hd with 
    | Term(n1, n2) ->  print_string "distribute term within plus to other plus\n" ; (distTerm li2 n1 n2) @ (distribute tl li2)
    | Times(_li) ->  
      begin
        match getTerm _li with
        | Term(n1,n2) -> (distTerm li2 n1 n2) @ (distribute tl li2)
        | _ -> print_string "ERROR" ; distribute tl li2
      end
    | _ -> print_string "distrbute error, plus inside plus?" ; (distribute tl li2)


and getTerm (elist: pExp list): pExp= 
match simplifyTimes elist with
| Term(n1, n2) -> Term(n1,n2)
| Times(tlist) -> getTerm tlist
| _ -> print_string "big fucky wucky" ; Term(-1, -1)


and flattenTimes (li: pExp list): pExp list =
  match li with
  | [] -> []
  | hd :: tl -> 
    match hd with 
    | Times(_li) -> (flattenTimes _li) @ (flattenTimes tl) 
    | Plus(_li) -> hd :: (flattenTimes tl)
    | Term(n1, n2) -> hd :: (flattenTimes tl)


and simplifyPlus (li:pExp list): pExp =
 match li with
 | Term(n1, n2) :: Term(n3, n4) :: []  -> print_string "Plus with exactly two terms\n" ; if n2 = n4 then Term (n1+n3, n2) else Plus(li)
 | Term(n1, n2) :: Term(n3, n4) :: li  -> print_string "Plus with at least two terms\n" ; 
  (* Combines first two terms *)
  if n2 = n4 then Plus ( Term (n1+n3, n2) :: li ) 
  else  (* Looks at 2nd and 3rd term, so on, recursively, relies on Plus being sorted *)
    begin (* ELEGANT AS FUCK DOBRA *)
      let simExp = simplify1 (Plus (Term(n3,n4) :: li) ) in
      match simExp with
      | Plus(newlist) -> Plus(Term(n1,n2) :: newlist)
      | _ -> Plus( [Term(n1,n2) ; simExp ] )
    end 
| hd :: [] -> print_string "Only one thing in plus\n" ; hd
| l -> let flat = flattenPlus l in Plus((findTimes flat []))

and flattenPlus (li: pExp list): pExp list =
  match li with
  | [] -> []
  | hd :: tl -> 
    match hd with 
    | Times(_li) -> hd :: (flattenPlus tl) 
    | Plus(_li) -> (flattenPlus _li) @ (flattenPlus tl)
    | Term(n1, n2) -> hd :: (flattenPlus tl)
    
and findTimes (origList: pExp list ) (newList: pExp list ) : (pExp list) =
 match origList with
 | [] -> print_string "original list is empty \n" ; newList 
 | (hd :: tl) -> 
  match hd with
  | Times(li) -> 
    begin 
      print_int (List.length li) ; print_string "\n" ;
      print_string "found times! \n" ; let list = ((simplifyTimes li) :: newList) in
      begin
        print_int (List.length list) ; print_string "\n" ; findTimes (List.tl origList) (list) 
      end
    end
  | _ -> print_string "searching for times! \n" ; findTimes (List.tl origList) (hd :: newList)



(* 
  Compute if two pExp are the same 
  Make sure this code works before you work on simplify1  
*)
let equal_pExp (_e1: pExp) (_e2: pExp) :bool =
  _e1 = _e2

(* Fixed point version of simplify1 
  i.e. Apply simplify1 until no 
  progress is made
*)    
let rec simplify (e:pExp): pExp =
  match e with
  | Plus(li) -> 
  begin
    let reducePlus = simplify1(Plus(flattenPlus (List.sort compare li))) in
      if (equal_pExp e reducePlus) then e
      else simplify(reducePlus) 
  end 
  | Times(li) ->
  begin
    let reduceTimes = simplify1(Times(flattenTimes li)) in
      if (equal_pExp e reduceTimes) then e
      else simplify(reduceTimes) 
  end
  | _ -> 
  let rE = simplify1(e) in
      (* print_pExp rE; *)
    if (equal_pExp e rE) then
      e
    else  
        simplify(rE)




