(* util functions *)

let is_lower_case c =
  'a' <= c && c <= 'z'

let is_upper_case c =
  'A' <= c && c <= 'Z'

let is_alpha c =
  is_lower_case c || is_upper_case c

let is_digit c =
  '0' <= c && c <= '9'

let is_alphanum c =
  is_lower_case c ||
  is_upper_case c ||
  is_digit c

let is_blank c =
  String.contains " \012\n\r\t" c

let explode s =
  List.of_seq (String.to_seq s)

let implode ls =
  String.of_seq (List.to_seq ls)

let readlines (file : string) : string =
  let fp = open_in file in
  let rec loop () =
    match input_line fp with
    | s -> s ^ "\n" ^ (loop ())
    | exception End_of_file -> ""
  in
  let res = loop () in
  let () = close_in fp in
  res

(* end of util functions *)

(* parser combinators *)

type 'a parser = char list -> ('a * char list) option

let parse (p : 'a parser) (s : string) : ('a * char list) option =
  p (explode s)

let pure (x : 'a) : 'a parser =
  fun ls -> Some (x, ls)

let fail : 'a parser = fun ls -> None

let bind (p : 'a parser) (q : 'a -> 'b parser) : 'b parser =
  fun ls ->
    match p ls with
    | Some (a, ls) -> q a ls
    | None -> None

let (>>=) = bind
let (let*) = bind

let read : char parser =
  fun ls ->
  match ls with
  | x :: ls -> Some (x, ls)
  | _ -> None
 
let satisfy (f : char -> bool) : char parser =
  fun ls ->
  match ls with
  | x :: ls ->
    if f x then Some (x, ls)
    else None
  | _ -> None

let char (c : char) : char parser =
  satisfy (fun x -> x = c)

let seq (p1 : 'a parser) (p2 : 'b parser) : 'b parser =
  fun ls ->
  match p1 ls with
  | Some (_, ls) -> p2 ls
  | None -> None

let (>>) = seq

let seq' (p1 : 'a parser) (p2 : 'b parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls) ->
    (match p2 ls with
     | Some (_, ls) -> Some (x, ls)
     | None -> None)
  | None -> None

let (<<) = seq'

let alt (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls)  -> Some (x, ls)
  | None -> p2 ls

let (<|>) = alt

let map (p : 'a parser) (f : 'a -> 'b) : 'b parser =
  fun ls ->
  match p ls with
  | Some (a, ls) -> Some (f a, ls)
  | None -> None

let (>|=) = map

let (>|) = fun p c -> map p (fun _ -> c)

let rec many (p : 'a parser) : ('a list) parser =
  fun ls ->
  match p ls with
  | Some (x, ls) ->
    (match many p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> Some ([], ls)

let rec many1 (p : 'a parser) : ('a list) parser =
  fun ls ->
  match p ls with
  | Some (x, ls) ->
    (match many p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> None

let rec many' (p : unit -> 'a parser) : ('a list) parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) ->
    (match many' p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> Some ([], ls)

let rec many1' (p : unit -> 'a parser) : ('a list) parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) ->
    (match many' p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> None

let whitespace : unit parser =
  fun ls ->
  match ls with
  | c :: ls ->
    if String.contains " \012\n\r\t" c
    then Some ((), ls)
    else None
  | _ -> None


let ws : unit parser =
  (many whitespace) >| ()

let ws1 : unit parser =
  (many1 whitespace) >| ()

let digit : char parser =
  satisfy is_digit

let natural : int parser =
  fun ls ->
  match many1 digit ls with
  | Some (xs, ls) ->
    Some (int_of_string (implode xs), ls)
  | _ -> None

let literal (s : string) : unit parser =
  fun ls ->
  let cs = explode s in
  let rec loop cs ls =
    match cs, ls with
    | [], _ -> Some ((), ls)
    | c :: cs, x :: xs ->
      if x = c
      then loop cs xs
      else None
    | _ -> None
  in loop cs ls

let keyword (s : string) : unit parser =
  (literal s) >> ws >| ()
  
(* end of parser combinators *)

(* Interprets a program written in the Part1 Stack Language.
 * Required by the autograder, do not change its type. *)

type const = 
  | Inty of int 
  | N of string 
  | Unit

type command = 
  | Push of const 
  | Trace
  | Add
  | Sub
  | Mul
  | Div
  | IfElseEnd of commands * commands
  and commands = command list

(* Parsing *)
let letter : char parser = 
  satisfy is_alpha

let letter2 : char parser = 
  satisfy is_alphanum

let name : string parser = 
  (char '_' <|> letter) >>= 
  fun hd -> many((letter2) <|> (char '_') <|> (char '\'')) >>=
  fun tl -> pure ((implode (hd :: tl)))

let num_parser : const parser =
  natural >>= 
  fun i -> let x = Inty(i) in pure(x) 

let unit : const parser = 
  (keyword "()" >>= fun i -> pure (Unit))


let const = 
  ((num_parser >>= fun i -> pure (i))<|>
  (name >>= fun i -> pure (N(i))) <|>
  (unit >>= fun i -> pure (Unit)))


let push_parser : command parser =
  (keyword "Push" >> const << ws >>= fun c -> pure (Push c))

let rec parse_command () =
  push_parser <|>
  (keyword "Trace" >> pure Trace) <|>
  (keyword "Add" >> pure Add) <|>
  (keyword "Sub" >> pure Sub) <|>
  (keyword "Mul" >> pure Mul) <|>
  (keyword "Div" >> pure Div) <|>
  (ifelseend())

  and ifelseend () = 
  keyword "If" >>= fun x -> parse_commands () >>= fun first -> 
  keyword "Else" >>= fun x -> parse_commands () >>= fun second -> 
  keyword "End" >> pure (IfElseEnd (first, second))

  and parse_commands () = many' parse_command  


(* Evaluating/Executing *)
type stack = const list 

let rec string_of_const_list (ls : stack) (accum) : string = 
  match ls with
  | [] -> accum
  | Unit::tail -> string_of_const_list tail "()"^accum
  | Inty(i) :: tail -> string_of_const_list tail accum^(string_of_int(i))
  | N( ";") :: tail -> string_of_const_list tail accum^";"
  | N(i) :: tail -> string_of_const_list tail accum^i

let string_of_const (c : const) : string = 
  match c with
  | Unit -> "()"
  | Inty(i) -> string_of_int(i)
  | N(i) -> i

let rec execute (p : commands) (stack : stack) (log : string list) : (string * string list) =
  match p, stack, log with
  | Push Inty(c) :: p, stack, log -> execute p (Inty(c) :: stack) log
  | Push N(c) :: p, stack, log -> execute p (N(c) :: stack) log
  | Push Unit :: p, stack, log -> execute p (Unit :: stack) log

  | Trace :: p, v1 :: stack, log -> execute p (Unit :: stack) (string_of_const(v1) :: log)
  | Trace :: p, _, log -> ("Error", [])

  | Add :: p, Inty(v2) :: Inty(v1) :: stack, log -> execute p (Inty(v1 + v2) :: stack) log
  | Add :: p, _, log -> ("Error", [])

  | Sub :: p, Inty(v2) :: Inty(v1) :: stack, log -> execute p (Inty(v1 - v2) :: stack) log
  | Sub :: p, _, log -> ("Error", [])

  | Mul :: p, Inty(v2) :: Inty(v1) :: stack, log -> execute p (Inty(v1 * v2) :: stack) log
  | Mul :: p, _, log -> ("Error", [])

  | Div :: p, Inty(v2) :: Inty(v1) :: stack, log -> (if Inty(v2) = Inty(0) then ("Error", []) else 
                                                execute p (Inty(v1 / v2) :: stack) log)
  | Div :: p, _, log -> ("Error", [])

  (*| IfElseEnd (c1, c2) :: p, Inty(v2) :: Inty(v1) :: stack, log -> 
    if Inty(v2) > Inty(0) then execute (c1@p) (Inty(v2) :: Inty(v1) :: stack) log 
    else execute (c2@p) (Inty(v2) :: Inty(v1) :: stack) log *)
  
  | IfElseEnd (c1, c2) :: p, Inty(v2) :: Inty(v1) :: stack, log -> 
    if Inty(v2) > Inty(0) then execute (c1@p) (Inty(v1) :: stack) log 
    else execute (c2@p) (Inty(v1) :: stack) log 
  | IfElseEnd (c1, c2) :: p, Inty(v1) :: stack, log -> 
    if Inty(v1) > Inty(0) then execute (c1@p) (stack) log 
    else execute (c2@p) (stack) log 
  | IfElseEnd (_, _) :: p, _, log -> ("Error", [])

  | [], v1::stack, _ -> (string_of_const_list [v1] "", log) 
  | _ -> ("Error", [])


let interpreter (src : string) : (string * string list) =
  match parse (parse_commands ()) src with
  | Some (h, []) -> execute h [] []
  | _ -> ("Error", [])
