-- Zech DeCleene  (ID: 932-466-139)
-- Lim Cheng Qing (ID: 933-317-774)

module HW3 where

import MiniMiniLogo
import Render


--
-- * Semantics of MiniMiniLogo
--

-- NOTE:
--  * MiniMiniLogo.hs defines the abstract syntax of MiniMiniLogo and some
--    functions for generating MiniMiniLogo programs. It contains the type
--    definitions for Mode, Cmd, and Prog.
--  * Render.hs contains code for rendering the output of a MiniMiniLogo
--    program in HTML5. It contains the types definitions for Point and Line.

-- | A type to represent the current state of the pen.
type State = (Mode,Point)

-- | The initial state of the pen.
start :: State
start = (Up,(0,0))

-- | A function that renders the image to HTML. Only works after you have
--   implemented `prog`. Applying `draw` to a MiniMiniLogo program will
--   produce an HTML file named MiniMiniLogo.html, which you can load in
--   your browswer to view the rendered image.
draw :: Prog -> IO ()
draw p = let (_,ls) = prog p start in toHTML ls


-- Semantic domains:
--   * Cmd:  State -> (State, Maybe Line)
--   * Prog: State -> (State, [Line])


-- | Semantic function for Cmd.
--
--   >>> cmd (Pen Down) (Up,(2,3))
--   ((Down,(2,3)),Nothing)
--
--   >>> cmd (Pen Up) (Down,(2,3))
--   ((Up,(2,3)),Nothing)
--
--   >>> cmd (Move 4 5) (Up,(2,3))
--   ((Up,(4,5)),Nothing)
--
--   >>> cmd (Move 4 5) (Down,(2,3))
--   ((Down,(4,5)),Just ((2,3),(4,5)))
--
cmd :: Cmd -> State -> (State, Maybe Line)
-- Change the state of the pen (up/down)
cmd (Pen Up) (_, (x,y))   = ((Up, (x,y)), Nothing)
cmd (Pen Down) (_, (x,y)) = ((Down, (x,y)), Nothing)
-- Move the pen while it's up
cmd (Move x1 y1) (Up, (_, _))   = ((Up, (x1, y1)), Nothing)
-- Move the pen while it's down (aka Draw)
cmd (Move x1 y1) (Down, (x2, y2)) = ((Down, (x1, y1)), Just((x1, y1), (x2, y2)))


-- | Semantic function for Prog.
--
--   >>> prog (nix 10 10 5 7) start
--   ((Down,(15,10)),[((10,10),(15,17)),((10,17),(15,10))])
--
--   >>> prog (steps 2 0 0) start
--   ((Down,(2,2)),[((0,0),(0,1)),((0,1),(1,1)),((1,1),(1,2)),((1,2),(2,2))])
prog :: Prog -> State -> (State, [Line])
prog c s = case c of
  [] -> (s, [])
  (command:theRest) -> case cmd command s of
    -- (State, Just Line) = draw
    (nextstate, Just line) -> (\(s, theRest) -> (s, line:theRest)) (prog theRest nextstate)
    -- (State, Nothing)   = just moving pen
    (nextstate, Nothing) -> prog theRest nextstate


--
-- * Extra credit
--

-- | This should be a MiniMiniLogo program that draws an amazing picture.
--   Add as many helper functions as you want.
-- Height = 40
-- Width  = 80
spiral :: Prog
spiral = spin 40 20 60 1

amazing :: Prog
amazing = line 12 0 24 12  ++ line 24 12 20 16 ++ line 20 16 4 16  ++ line 4 16 0 12   ++
          line 0 12 12 0   ++ line 2 12 4 10   ++ line 4 10 4 12   ++ line 4 12 5 14   ++
          line 5 14 7 15   ++ line 7 15 5 15   ++ line 5 15 2 12   ++ line 12 2 15 5   ++
          line 15 5 9 5    ++ line 9 5 12 2    ++ line 5 9 8 8     ++ line 8 8 12 8    ++
          line 12 8 16 7   ++ line 16 7 14 6   ++ line 14 6 12 6   ++ line 12 6 10 7   ++
          line 10 7 8 6    ++ line 8 6 5 9     ++ line 19 9 22 12  ++ line 22 12 20 14 ++
          line 20 14 20 12 ++ line 20 12 16 12 ++ line 16 12 14 14 ++ line 14 14 12 15 ++
          line 12 15 8 14  ++ line 8 14 7 13   ++ line 7 13 8 12   ++ line 8 12 12 11  ++
          line 12 11 16 11 ++ line 16 11 19 9  ++ line 16 15 17 15 ++ line 17 15 17 14 ++
          line 17 14 16 15

line :: Int -> Int -> Int -> Int -> Prog
line x1 y1 x2 y2 = [Pen Up, Move x1 y1, Pen Down, Move x2 y2, Pen Up]

spin :: Int -> Int -> Int -> Int -> Prog
spin _ _ 0 _ = []
spin x y n l = if mod n 4 == 0 then [Pen Up, Move x y, Pen Down, Move (x+l) (y+l), Pen Up] ++ spin (x+l) (y+l) (n-1) (l+1)
               else if mod n 4 == 1 then [Pen Up, Move x y, Pen Down, Move (x-l) (y+l), Pen Up] ++ spin (x-l) (y+l) (n-1) (l+1)
               else if mod n 4 == 2 then [Pen Up, Move x y, Pen Down, Move (x-l) (y-l), Pen Up] ++ spin (x-l) (y-l) (n-1) (l+1)
               else [Pen Up, Move x y, Pen Down, Move (x+l) (y-l), Pen Up] ++ spin (x+l) (y-l) (n-1) (l+1)
