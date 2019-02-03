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
-- Define the abstract syntax of MiniLogo as a set of Haskell data types.
-- You should use built-in types for num, var, and macro. (If you want to define
-- a type Num, you will have to hide that name from the Prelude).

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

-- 2.
-- Define a MiniLogo macro line (x1,y1,x2,y2) that (starting from anywhere on
-- the canvas) draws a line segment from (x1,y1) to (x2,y2).
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
-- Use the line macro you just defined to define a new MiniLogo
-- macro nix (x,y,w,h) that draws a big “X” of width w and height h, starting
-- from position (x,y). Your definition should not contain any move commands.
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
