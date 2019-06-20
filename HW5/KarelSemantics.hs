-- Zech DeCleene  ID:932-466-139
-- Lim cheng Qing ID:933-317-774
module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState


-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test (Not t)      w r = not (test t w r)              -- Not test
test (Facing c)   _ r = c == (getFacing r)            -- Facing Cardinal Direction
test (Clear d)    w r = (isClear (relativePos d r) w) -- Clear Dir
test (Beeper)     w r = (hasBeeper (getPos r) w)      -- Current location has a beeper?
test (Empty)      w r = (isEmpty r)

-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ r = Done r

stmt Move     _ w r =
    if test (Clear Front) w r
    then OK w (updatePos (neighbor (getFacing r)) r)
    else Error ("Blocked is at: " ++ show (relativePos Front r))


stmt PickBeeper _ w r = let p = getPos r
                        in if hasBeeper p w
                              then OK (decBeeper p w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show p)

stmt PutBeeper  _ w r = if test Empty w r
                        then Error "No beeper to put."
                        else OK (incBeeper (getPos r) w) (decBag r)

stmt (Turn x)   _ w r = OK w (setFacing (cardTurn x (getFacing r)) r)

stmt (If t s1 s2)   d w r = if (test t w r)
                            then (stmt s1 d w r)
                            else (stmt s2 d w r)

stmt (Call x)   d w r = case (lookup x d) of
                        Just s -> (stmt s d w r)
                        Nothing -> Error ("Error! Undefined macro: " ++ x)


stmt (Iterate x y)  d w r = if x > 1
                                        then case (stmt y d w r) of
                                            OK    w' r' -> stmt (Iterate (x-1) y) d w' r'
                                            Error e     -> Error e
                                            Done  r'    -> Done r'
                                        else stmt y d w r



stmt (While x y) d w r = if (test x w r)
                        then case stmt y d w r of
                        OK w' r' -> stmt (While x y) d w' r'
                        Done r'  -> Done r'
                        Error e  -> Error e
                        else OK w r

stmt (Block (x:xs)) d w r = let p = stmt x d w r in
                            case p of
                            OK w' r' -> stmt (Block xs) d w' r'
                            Done r'  -> Done r'
                            Error e  -> Error e

stmt (Block []) d w r = OK w r


-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
