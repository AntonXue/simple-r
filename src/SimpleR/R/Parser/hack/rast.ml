type source =
  { file : string
  ; line : int
  ; col  : int }


type 'a ident =
  { pkg  : string option
  ; name : string
  ; src  : source option
  ; tag  : 'a option }


let default_ident =
  { pkg  = None
  ; name = ""
  ; src  = None
  ; tag  = None }

(*
type numeric =
    Int of int
  | Float of float
  | Complex of float * float
  | Na
*)


type unop =
    UMinus (* - *)
  | UPlus  (* + *)
  | Not    (* ! *)
  | UForm  (* ~ *)
  | UHelp  (* ? *)


type binop =
  (* Numerical *)
    Plus
  | Minus
  | Mult
  | Div
  | Pow
  | Mod
  | IntDiv
  | MatrixMult
  | OuterProd
  | KronProd
  | Match
  (* Boolean *)
  | Gt
  | Ge
  | Lt
  | Le
  | Eq
  | Neq
  | AndVec
  | And
  | Or
  | OrVec
  (* Assignment *)
  | AssignArrow
  | SuperAssignArrow
  (* List access *)
  | ObjAttrGet
  (* List ranges *)
  | Range
  (* What the hell is this *)
  | Form
  (* Qualified namespace lookup *)
  | GetPackage
  | GetPackageInt
  (* Help?? *)
  | Help
  | ColonAssign

type rnumeric =
    NumInt of int
  | NumNaInt
  | NumFloat of float
  | NumNaFloat
  | NumComplex of float * float
  | NumNaComplex

type rbool =
    RBool of bool
  | RNaBool

type rstring =
    RString of string
  | RNaString

type rconst =
    NumConst of rnumeric
  | StrConst of rstring
  | BoolConst of rbool
  | NaConst

type 'a arg =
  (* Expression *)
    ExprArg of 'a expr
  (* Assignments *)
  | IdentAssignEmpty of 'a ident
  | IdentAssign of 'a ident * 'a expr
  | StringAssignEmpty of string
  | StringAssign of string * 'a expr
  | NullAssignEmpty
  | NullAssign of 'a expr
  (* Variadic argument *)
  | ArgDots


and 'a param =
    Param of 'a ident
  | DefaultParam of 'a ident * 'a expr
  | ParamDots


and 'a expr =
  (* Constants *)
  | Const of rconst
  | Null
  (* Identifiers *)
  | Ident of 'a ident
  (* Unary and binary operators *)
  | Uop of unop * 'a expr
  | Bop of binop * 'a expr * 'a expr
  (* Function declaration and calls *)
  | FunCall of 'a expr * ('a arg) list
  | FunDef of ('a param) list * 'a expr
  (* Expression blocks *)
  | Block of 'a expr list            (* {} *)

  (* Assignment *)
  (*
    | Assign of 'a expr * 'a expr
    | SuperAssign of 'a expr * 'a expr
  *)

  (* Access Operations *)
  | ListProj of 'a expr * ('a arg) list
  | ListSub of 'a expr * ('a arg) list
  | ListName of 'a expr * 'a expr
  | ObjAttr of 'a expr * 'a expr
  (* Control structures *)
  | If of 'a expr * 'a expr (* empty block is like {NA}?*)
  | IfElse of 'a expr * 'a expr * 'a expr
  | For of ('a ident * 'a expr) * 'a expr
  | While of 'a expr * 'a expr
  | Repeat of 'a expr
  | Next
  | Break
  (* ? *)


type 'a program = ('a expr) list


(* Useful string conversion functions *)

let string_of_float2 : float -> string =
  fun f ->
    let str = string_of_float f in
    let len = String.length str in
      if str.[len - 1] = '.' then
        str ^ "0"
      else if str = "inf" then
        "Infinity"
      else if str = "-inf" then
        "-Infinity"
      else
        str

let string_of_ident : 'a ident -> string =
  fun id -> match id.pkg with
    | None ->
      "RIdent {ridPkg = Nothing, ridName = \"" ^ id.name ^ "\", ridSrc = Nothing, ridAnnot = Nothing}"
    | Some pkg -> 
      "RIdent {ridPkg = Just \"" ^ pkg ^ "\", ridName = \"" ^ id.name ^ "\", ridSrc = Nothing, ridAnnot = Nothing}"

(*
let string_of_numeric : numeric -> string =
  function
    | Na             -> "NA"
    | Int i          -> "Int " ^ (string_of_int i)
    | Float f        -> "Float " ^ (string_of_float f)
    | Complex (r, i) -> "Complex (" ^ (string_of_float r) ^ ", " ^
                                      (string_of_float i) ^ ")"
*)

let string_of_unop : unop -> string =
  function
    | UMinus -> "RUMinus"
    | UPlus  -> "RUPlus"
    | Not    -> "RUNot"
    | UForm  -> "RUForm"
    | UHelp  -> "RUHelp"


let string_of_binop : binop -> string =
  function
    | Plus          -> "RPlus"
    | Minus         -> "RMinus"
    | Mult          -> "RMult"
    | Div           -> "RDiv"
    | Pow           -> "RPow"
    | Mod           -> "RMod"
    | IntDiv        -> "RIntDiv"
    | MatrixMult    -> "RMatrixMult"
    | OuterProd     -> "ROuterProd"
    | KronProd      -> "RKronProd"
    | Match         -> "RMatch"
    | Gt            -> "RGt"
    | Ge            -> "RGe"
    | Lt            -> "RLt"
    | Le            -> "RLe"
    | Eq            -> "REq"
    | Neq           -> "RNeq"
    | And           -> "RAnd"
    | AndVec        -> "RAndVec"
    | Or            -> "ROr"
    | OrVec         -> "ROrVec"
    | Form          -> "RForm"
    | AssignArrow   -> "RAssignArrow"
    | SuperAssignArrow -> "RSuperAssignArrow"
    | ObjAttrGet    -> "RObjAttrGet"
    | Range         -> "RRange"
    | Help          -> "RHelp"
    | GetPackage    -> "RGetPackage"
    | GetPackageInt -> "RGetPackageInt"
    | ColonAssign   -> "RColonAssign"


let string_of_rnumeric =
  function
    | NumInt i -> "RNumInt " ^ string_of_int i
    | NumNaInt -> "RNumNaInt"
    | NumFloat f -> "RNumFloat " ^ string_of_float2 f
    | NumNaFloat -> "RNumNaFloat"
    | NumComplex (r, i) -> "RNumComplex (" ^ string_of_float2 r ^ " :+ " ^ string_of_float2 i ^ ")"
    | NumNaComplex -> "RNumNaComplex"

let string_of_rbool =
  function
    | RBool true -> "RBool True"
    | RBool false -> "RBool False"
    | RNaBool -> "RNaBool"

let string_of_rstring =
  function
    | RString s -> "RString \"" ^ s ^ "\""
    | RNaString -> "RNaString"

let string_of_rconst =
  function
    | NumConst n -> "RNumConst (" ^ string_of_rnumeric n ^ ")"
    | StrConst s -> "RStrConst (" ^ string_of_rstring s ^ ")"
    | BoolConst b -> "RBoolConst (" ^ string_of_rbool b ^ ")"
    | NaConst -> "RNaConst"


let rec string_of_expr : 'a expr -> string =
  function
    (* Values *)
    | Const c -> "RConst (" ^ string_of_rconst c ^ ")"
    | Ident i -> "RVar (" ^ string_of_ident i ^ ")"
    | Null           -> "RNull"

    (* Operators *)
    | Uop (u, e) ->
        "RUnOp (" ^ string_of_unop u ^ ") (" ^ string_of_expr e ^ ")"

    | Bop (b, e1, e2) ->
        "RBinOp (" ^ string_of_binop b ^ ") (" ^
                 string_of_expr e1 ^ ") (" ^ string_of_expr e2 ^ ")"
    (* Functions *)
    | FunCall (e, args) ->
        "RFunCall (" ^
         string_of_expr e ^ ") [" ^
         (String.concat "," (List.map string_of_arg args)) ^ "]"
    | FunDef (ps, e) ->
        "RFunDef [" ^
          (String.concat "," (List.map string_of_param ps)) ^ "] (" ^
          string_of_expr e ^ ")"

    (* Block of expressions *)
    | Block (es) ->
        "RSeq [" ^ (String.concat "," (List.map string_of_expr es)) ^ "]"
    (* Control expressions *)
    | If (c, et) ->
        "RIf (" ^ string_of_expr c ^ ") (" ^ string_of_expr et ^ ")"
    | IfElse (c, et, ef) ->
        "RIfElse (" ^ string_of_expr c ^ ") (" ^
                    string_of_expr et ^ ") (" ^ string_of_expr ef ^ ")"
    | For ((i, e2), e3) ->
        "RFor (" ^ string_of_ident i ^ ") (" ^ string_of_expr e2 ^ ") (" ^
                 string_of_expr e3 ^ ")"
    | While (e1, e2) ->
        "RWhile (" ^ string_of_expr e1 ^ ") (" ^ string_of_expr e2 ^ ")"
    | Repeat (e) -> "RRepeat (" ^ string_of_expr e ^ ")"
    | Next -> "RNext"
    | Break -> "RBreak"

    (* Assignment *)
    (*
      | Assign (e1, e2) ->
          "RAssign (" ^ string_of_expr e1 ^ ") (" ^ string_of_expr e2 ^ ")"
      | SuperAssign (e1, e2) ->
          "RSuperAssign (" ^ string_of_expr e1 ^ ") (" ^ string_of_expr e2 ^ ")"
    *)

    (* List acessing *)
    | ListProj (e, args) ->
        "RVecProj (" ^
          string_of_expr e ^ ") [" ^
          (String.concat "," (List.map string_of_arg args)) ^ "]"
    | ListSub (e, args) ->
        "RVecSub (" ^
          string_of_expr e ^ ") [" ^
          (String.concat "," (List.map string_of_arg args)) ^ "]"
    | ListName (e1, e2) ->
        "RListName (" ^ string_of_expr e1 ^ ") (" ^ string_of_expr e2 ^ ")"
    | ObjAttr (e1, e2) ->
        "RObjAttr (" ^ string_of_expr e1 ^ ") (" ^ string_of_expr e2 ^ ")"

and string_of_arg : 'a arg -> string =
  function
    | ExprArg e           -> "RExprArg (" ^ string_of_expr e ^ ")"
    | IdentAssign (i, e)  -> "RIdentAssign (" ^ string_of_ident i ^ ") (" ^
                              string_of_expr e ^ ")"
    | IdentAssignEmpty i  -> "RIdentAssignEmpty (" ^ string_of_ident i ^ ")"
    | StringAssign (s, e) -> "RStringAssign (RString \"" ^ s ^ "\") (" ^
                                string_of_expr e ^ ")"
    | StringAssignEmpty s -> "RStringAssignEmpty (RString \"" ^ s ^ "\")"
    | NullAssign e        -> "RNullAssign (" ^ string_of_expr e ^ ")"
    | NullAssignEmpty     -> "RNullAssignEmpty"
    | ArgDots             -> "RVarArg"


and string_of_param : 'a param -> string =
  function
    | Param i             -> "RParam (" ^ string_of_ident i ^ ")"
    | DefaultParam (i, e) -> "RDefault (" ^ string_of_ident i ^ ") (" ^
                                            string_of_expr e ^ ")"
    | ParamDots           -> "RVarParam"


let string_of_program : 'a program -> string =
  fun es ->
    "RProgram [" ^ (String.concat "," (List.map string_of_expr es)) ^ "]"


