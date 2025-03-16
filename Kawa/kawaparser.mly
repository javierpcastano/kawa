%{

  open Lexing
  open Kawa

%}

%token <int> INT
%token <string> IDENT
%token MAIN
%token LPAR RPAR BEGIN END SEMI DOT COMMA
%token PRINT
%token EOF
%token TRUE FALSE
%token BOOL VOID T_INT
%token ATTRIBUTE METHOD CLASS NEW EXTENDS
%token IF WHILE ELSE
%token RETURN
%token THIS
%token ADD SUB MUL DIV
%token AS EQ NEQ
%token GT LT GE LE 
%token AND OR
%token REM
%token NOT
%token VAR 
%token EQS NEQS (*extension égalité structurelle*)



%nonassoc EQ NEQ EQS NEQS LT LE GT GE (* Comparaisons, priorité faible mais au-dessus de l'affectation *)
%left OR (*OR a une priorité plus faible que AND  *)
%left AND (* AND a une priorité plus faible que les comparaisons  *)
%right NOT (*NOT est unaire, priorité plus haute qu'AND  *)
%left ADD SUB (*Addition et soustraction ont la même priorité  *)
%left MUL DIV REM (*Multiplication, division et reste ont une priorité plus haute  *)
%left DOT (*accès aux membres a une priorité élevée  *)




%start program
%type <Kawa.program> program


%%

program:
|globals = var_decl* classes=class_def* MAIN BEGIN main=list(instruction) END EOF
    { {classes; globals; main} }
;
class_def:
|CLASS id=IDENT BEGIN a=attr_decl* m=method_def* END
    { { class_name = id; attributes=a; methods=m; parent = None } }
|CLASS id=IDENT EXTENDS p=IDENT BEGIN a=attr_decl* m=method_def* END
    { { class_name = id; attributes=a; methods=m; parent = Some p } }
;


var_decl:
| VAR t=types id=IDENT SEMI {(id, t)}
;




attr_decl:
| ATTRIBUTE t=types id= IDENT SEMI { (id, t) }
;


types:
| BOOL { TBool }        
| T_INT { TInt }              
| VOID { TVoid }           
| id=IDENT { TClass(id) }    
;

method_def:
| METHOD t=types id=IDENT LPAR p=separated_list(COMMA,param) RPAR BEGIN loc=var_decl* c=instruction* END
    { { method_name = id; code = c; params=p; locals=loc; return = t } }
;


param: 
t=types id=IDENT {(id, t)}
;



expression:
| n=INT { Int(n) }
| TRUE { Bool(true) }
| FALSE { Bool(false) }
| THIS {This} 
| m=memoire {Get(m)}
| up = unop e = expression {Unop(up , e)}
| e1=expression op=binop e2=expression { Binop(op, e1, e2) }
| LPAR e=expression RPAR  {e}
| NEW id = IDENT {New (id)}
| NEW id = IDENT LPAR e=separated_list(COMMA,expression) RPAR {NewCstr(id, e)} 
| e=expression DOT id=IDENT LPAR le=separated_list(COMMA,expression) RPAR {MethCall(e, id, le)}

;

memoire:
|id=IDENT {Var(id)}
|e=expression DOT id=IDENT {Field(e, id)}
;

instruction:
| PRINT LPAR e=expression RPAR SEMI { Print(e) }
| m=memoire AS e=expression SEMI {Set(m,e)}
| IF LPAR e=expression RPAR BEGIN if_instr=list(instruction) END ELSE BEGIN else_instr=list(instruction) END {If(e, if_instr, else_instr)}
| WHILE LPAR e=expression RPAR BEGIN isntr= list(instruction) END {While (e, isntr)}
| RETURN e= expression SEMI {Return (e)}
| e=expression SEMI {Expr(e)}
;


%inline unop:
| NOT { Not }
| SUB { Opp }

%inline binop:

| ADD { Add }
| SUB { Sub }
| MUL { Mul }
| DIV { Div }
| REM { Rem }
| EQ { Eq }
| NEQ { Neq }
| LT { Lt }
| LE { Le }
| GT { Gt }
| GE { Ge }
| AND { And }
| OR { Or }

| EQS { Eqs } (* extension égalité structurelle *)
| NEQS { Neqs } (* extension égalité structurelle *)

