-- Lim Cheng Qing (ID: 933-317-774)
-- Zech DeCleene  (ID: 932-466-139)

module HW3 where
import Data.List

-- num	::=	(any natural number)
-- var	::=	(any variable name)
-- macro	::=	(any macro name)
--
-- prog	::=	ε   |   cmd ; prog	sequence of commands
--
-- mode	::=	down   |   up	pen status
--
-- expr	::=	var	variable reference
-- |	num	literal number
-- |	expr + expr	addition expression
--
-- cmd	::=	pen mode	change pen mode
-- |	move ( expr , expr )	move pen to a new position
-- |	define macro ( var* ) { prog }  	define a macro
-- |	call macro ( expr* )	invoke a macro

-- 1.
-- Define the abstract syntax of MiniLogo as a set of Haskell data types. You should use built-in types
-- for num, var, and macro. (If you want to define a type Num, you will have to hide that name from the Prelude).

type Prog = [Cmd]
type Macro = String
type Var = String

data Expr = Variable Var
          | Num Int
          | Expr2 Expr Expr
          deriving (Eq, Show)

data Mode = Down | Up
          deriving (Eq, Show)

data Cmd  = Pen Mode
          | Move (Expr , Expr)
          | Define Macro [Var] Prog
          | Call Macro [Expr]
          deriving (Eq, Show)

--[Pen Up,Move 10 10,Pen Down,Move 15 17,Pen Up,Move 10 17,Pen Down,Move 15 10]

--[Pen Up,Move 10 10, Define "qing", ["Stringx"], [Pen Up,Move 2 3]]

-- 2.
-- Define a MiniLogo macro line (x1,y1,x2,y2) that (starting from anywhere on the canvas) draws a line
-- segment from (x1,y1) to (x2,y2).
--
--      2a.
--      First, write the macro in MiniLogo concrete syntax (i.e. the notation
--      defined by the grammar and used in the example programs above). Include
--      this definition in a comment in your submission.
--
--      define Line (x1, y1, x2, y2) {
--          Pen up;
--          Move (x1, y1);
--          Pen down;
--          Move (x2, y2);
--          Pen up;
--      }
--
--
--      2b.
--      Second, encode the macro definition as a Haskell value using the data
--      types defined in Task 1. This corresponds to the abstract syntax of
--      MiniLogo. Your Haskell definition should start with something like
--      line = Define "line" ...

line = Define "line" ["x1", "y1", "x2", "y2"] [
     Pen Up,
     Move (Variable "x1", Variable "y1"),
     Pen Down,
     Move (Variable "x2", Variable "y2"),
     Pen Up
     ]

-- 3.
-- Use the line macro you just defined to define a new MiniLogo macro nix (x,y,w,h) that draws a big “X”
-- of width w and height h, starting from position (x,y). Your definition should not contain any move commands.
--
--      3a.
--      First, write the macro in MiniLogo concrete syntax and include this
--      definition in a comment in your submission.
--
--      Making the bottom left corner of the 'X' = (x   ,   y)
--                 bottom right                    (x+w ,   y)
--                 top    left                     (x   , y+h)
--                 rop    right                    (x+w , y+h)
--
--      Define nix (x, y, w, h) {
--          line (x, y, x+w, y+h)
--          line (x, y+h, x+w, y)
--      }
--
--      3b.
--      Second, encode the macro definition as a Haskell value, representing
--      the abstract syntax of the definition.
nix = Define "nix" ["x", "y", "w", "h"] [
    Call "line" [Variable "x", Variable "y", Expr2 (Variable "x") (Variable "w"), Expr2 (Variable "y") (Variable "h")],
    Call "line" [Variable "x", Expr2 (Variable "y") (Variable "h"), Expr2 (Variable "x") (Variable "w"), Variable "y"]
    ]

-- 4.
-- Define a Haskell function steps :: Int -> Prog that constructs a MiniLogo program that draws a staircase
-- of n steps starting from (0,0). Below is a visual illustration of what the generated program should draw
-- for a couple different applications of steps.

steps :: Int -> Prog
steps 0 = []
steps i = [
          Call "line" [Num i, Num i, Num (i - 1), Num i],
          Call "line" [Num (i - 1), Num i, Num (i - 1), Num (i - 1)]
          ] ++ steps (i-1)

-- 5.
-- Define a Haskell function macros :: Prog -> [Macro] that returns a list of the names of all of the macros that
-- are defined anywhere in a given MiniLogo program. Don’t worry about duplicates—if a macro is defined more than
-- once, the resulting list may include multiple copies of its name.



macros :: Prog -> [Macro]
macros [] = []    --returns nothing if nothing
macros (p:ps) = case p of
  Define m _ _ -> m:macros ps
  otherwise -> macros ps



-- 6.
-- Define a Haskell function pretty :: Prog -> String that pretty-prints a MiniLogo
-- program. That is, it transforms the abstract syntax (a Haskell value) into nicely
-- formatted concrete syntax (a string of characters). Your pretty-printed program should
-- look similar to the example programs given above; however, for simplicity you will
--  probably want to print just one command per line.

--In GHCi, you can render a string with newlines by applying the function putStrLn.
-- So, to pretty-print a program p use: putStrLn (pretty p).

-- A helper function to pretty print a variable


-- *HW3>

-- >>>let a = Define "nix" ["x", "y", "w", "h"] [Call "line1" [Variable "x", Variable "y", Expr2 (Variable "z") (Variable "a"), Expr2 (Num 23) (Variable "b")],Call "line2" [Expr2 (Variable "c") (Variable "d"), Variable "e", Variable "f", Expr2 (Num 19) (Num 21)]]
-- >>>pretty [a]
-- Result
--"Define nix(x, y, w, h) {line1(x, y, z + a, 23 + b); line2(c + d, e, f, 19 + 21); }; "


-- >>>let b = Define "nix" ["x", "y", "w", "h"] [Call "line" [Variable "x", Variable "y", Expr2 (Variable "z") (Variable "a"), Expr2 (Num 23) (Variable "b")],Call "line" [Expr2 (Variable "c") (Variable "d"), Variable "e", Variable "f", Expr2 (Num 19) (Num 21)]]
-- >>>pretty [b]
-- Result
--"Define nix(x, y, w, h) {line(x, y, z + a, 23 + b); line(c + d, e, f, 19 + 21); }; "


prettyPrint :: Expr -> String
prettyPrint (Num number) = show number
prettyPrint (Variable stringValue) = stringValue
prettyPrint (Expr2 left right) = prettyPrint left ++ " + " ++ prettyPrint right


pretty :: Prog -> String
pretty [] = ""
pretty (Pen Up:x) = "Pen-up; " ++ pretty x
pretty (Pen Down:x) = "Pen-up; " ++ pretty x
pretty (Move (left, right):x) = "Move (" ++ prettyPrint left ++ ", " ++ prettyPrint right ++ "); " ++ pretty x
pretty (Call n y:x) = n ++ "(" ++ intercalate ", " (map prettyPrint y) ++ "); " ++ pretty x
pretty (Define m y p:z) = "Define " ++ m ++ "(" ++ intercalate ", " y ++ ") {" ++ pretty p ++ "}; " ++ pretty z


-- 7.
-- Define a Haskell function optE :: Expr -> Expr that partially evaluates expressions by
--  replacing any additions of literals with the result. For example, given the expression
-- (2+3)+x, optE should return the expression 5+x.

-- Test Case

-- >>>optE (Expr2 (Num 3) (Num 2))
-- Result
-- Num 5

-- >>>optE (Expr2 (Num 10) (Expr2 (Variable "a") (Num 12)))
-- Result
-- Expr2 (Num 10) (Expr2 (Variable "a") (Num 12))


optE :: Expr -> Expr
optE (Expr2 (Num x) (Num y)) = Num $ x + y
optE otherwise = otherwise

-- 8.
-- Define a Haskell function optP :: Prog -> Prog that optimizes all of the expressions
-- contained in a given program using optE.

-- >>>let b = Define "nix" ["x", "y", "w", "h"] [Call "line" [Variable "x", Variable "y", Expr2 (Variable "z") (Variable "a"), Expr2 (Num 23) (Variable "b")],Call "line" [Expr2 (Variable "c") (Variable "d"), Variable "e", Variable "f", Expr2 (Num 19) (Num 21)]]
-- >>>optP [b]
-- Result
-- [Define "nix" ["x","y","w","h"] [Call "line" [Variable "x",Variable "y",Expr2 (Variable "z") (Variable "a"),Expr2 (Num 23) (Variable "b")],Call "line" [Expr2 (Variable "c") (Variable "d"),Variable "e",Variable "f",Num 40]]]

-- >>>let a = Define "nix" ["x", "y", "w", "h"] [Call "line1" [Variable "x", Variable "y", Expr2 (Variable "z") (Variable "a"), Expr2 (Num 23) (Variable "b")],Call "line2" [Expr2 (Variable "c") (Variable "d"), Variable "e", Variable "f", Expr2 (Num 19) (Num 21)]]
-- >>> optP [a]
--   Result
--- [Define "nix" ["x","y","w","h"] [Call "line1" [Variable "x",Variable "y",Expr2 (Variable "z") (Variable "a"),Expr2 (Num 23) (Variable "b")],Call "line2" [Expr2 (Variable "c") (Variable "d"),Variable "e",Variable "f",Num 40]]]

optP :: Prog -> Prog
optP [] = []
optP (p:s) = case p of
  Move (left, right) -> Move (optE left, optE right):optP s
  Call x z -> Call x (map optE z):optP s
  Define x vs p -> Define x vs (optP p):optP s
  otherwise -> p:optP s
