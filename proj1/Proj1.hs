 -- Author: Wenkai Huang <whhuang@student.unimelb.edu.au>
 -- ID: 991257
 -- Purpose: Perform a Musician game between composer and performer.

 -- In this logical guessing game, the performer guess 
 -- a three-pitch musical chord which select by the composer before. 
 -- The each pitch consists of a musical note from one of 
 -- A, B, C, D, E, F and G and an octave from 1, 2, 3. We need to 
 -- write some functions like toPitch, feed back, GameState, 
 -- ititialGuess, nextGuess and others to realize these guessing game. 
 -- The codes are shown in the below.
module Proj1 (Pitch, toPitch, feedback, GameState, initialGuess, nextGuess) where

import Data.List

 -- Define a data type which type constructor is Note and data constructor 
 --is either A, B, C, D, E, F, G to denote the Note.
data Note = A|B|C|D|E|F|G

 -- The type of Octave is Int.
type Octave = Int

 -- The Pitch data type consists of two arguments which are Note and Octave.
data Pitch = Pitch (Note, Octave)

 -- This function transfer the data type Pitch to the String
showPitch :: Pitch -> String
showPitch (Pitch (A, 1)) = "A1"
showPitch (Pitch (A, 2)) = "A2"
showPitch (Pitch (A, 3)) = "A3"
showPitch (Pitch (B, 1)) = "B1"
showPitch (Pitch (B, 2)) = "B2"
showPitch (Pitch (B, 3)) = "B3"
showPitch (Pitch (C, 1)) = "C1"
showPitch (Pitch (C, 2)) = "C2"
showPitch (Pitch (C, 3)) = "C3"
showPitch (Pitch (D, 1)) = "D1"
showPitch (Pitch (D, 2)) = "D2"
showPitch (Pitch (D, 3)) = "D3"
showPitch (Pitch (E, 1)) = "E1"
showPitch (Pitch (E, 2)) = "E2"
showPitch (Pitch (E, 3)) = "E3"
showPitch (Pitch (F, 1)) = "F1"
showPitch (Pitch (F, 2)) = "F2"
showPitch (Pitch (F, 3)) = "F3"
showPitch (Pitch (G, 1)) = "G1"
showPitch (Pitch (G, 2)) = "G2"
showPitch (Pitch (G, 3)) = "G3"


 -- The following three functions make the instance for each variable.
instance Eq Note where
  A == A = True
  B == B = True
  C == C = True
  D == D = True
  E == E = True
  F == F = True
  G == G = True
  _ == _ = False

instance Eq Pitch where 
  Pitch (w1, w2) == Pitch (z1, z2) = ((w1 == z1) && (z2 == z2))

instance Show Pitch where show  =  showPitch


 -- The toPitch function tests whether the Pitch is the valid or not.
toPitch :: String -> Maybe Pitch
toPitch str
 | (str == "A1") = Just (Pitch (A, 1))
 | (str == "A2") = Just (Pitch (A, 2))
 | (str == "A3") = Just (Pitch (A, 3))
 | (str == "B1") = Just (Pitch (B, 1))
 | (str == "B2") = Just (Pitch (B, 2))
 | (str == "B3") = Just (Pitch (B, 3))
 | (str == "C1") = Just (Pitch (C, 1))
 | (str == "C2") = Just (Pitch (C, 2))
 | (str == "C3") = Just (Pitch (C, 3))
 | (str == "D1") = Just (Pitch (D, 1))
 | (str == "D2") = Just (Pitch (D, 2))
 | (str == "D3") = Just (Pitch (D, 3))
 | (str == "E1") = Just (Pitch (E, 1))
 | (str == "E2") = Just (Pitch (E, 2))
 | (str == "E3") = Just (Pitch (E, 3))
 | (str == "F1") = Just (Pitch (F, 1))
 | (str == "F2") = Just (Pitch (F, 2))
 | (str == "F3") = Just (Pitch (F, 3))
 | (str == "G1") = Just (Pitch (G, 1))
 | (str == "G2") = Just (Pitch (G, 2))
 | (str == "G3") = Just (Pitch (G, 3))
 | otherwise = Nothing
 

  -- The inputs of this function are target and guess 
  -- and return the feedback which is the number of pitches, notes and octaves.
feedback :: [Pitch] -> [Pitch] -> (Int, Int, Int)
feedback [] [] = (0, 0, 0)
feedback [] _ = (0, 0, 0)
feedback _ [] = (0, 0, 0)
feedback (Pitch (x1,x2):xs) (Pitch (y1,y2):ys) =
 let filter_x = function_a (Pitch (y1,y2):ys) (Pitch (x1,x2):xs)
     filter_y = function_a (Pitch (x1,x2):xs) (Pitch (y1,y2):ys) in
       let m = get_value_pitch filter_x in
         function_p m filter_x filter_y
 
 -- The same pitches from target and guess 
 -- are deleted by the funtion_a and funtion_b.
function_a :: [Pitch] -> [Pitch] ->[Pitch]
function_a (Pitch (x1,x2):xs) [] = []
function_a (Pitch (x1,x2):xs) (Pitch (y1,y2):ys) =
 let happ = function_b (Pitch (x1,x2):xs) (Pitch (y1,y2))  in
   case happ of
     Pitch(a1, a2) -> 
              if (a1==A&&a2==0) then (function_a (Pitch (x1,x2):xs) ys)
              else Pitch(a1,a2):function_a (Pitch(x1,x2):xs) ys


function_b :: [Pitch] -> Pitch -> Pitch
function_b [] (Pitch (y1,y2)) = Pitch(y1,y2)
function_b (Pitch (x1, x2):xs) (Pitch (y1,y2)) =
 if (x1 == y1) && (x2 == y2) then (Pitch(A,0))
 else function_b xs (Pitch (y1,y2))

 -- This function returns the value of pitch, 
 -- note and octave from filter_x and filter_y.
function_p :: Int -> [Pitch] -> [Pitch] ->(Int, Int, Int)
function_p m [] [] = (m, 0, 0)
function_p m (p:ps) (q:qs) =
  let  n1 = get_value_Note ((get_Note (p:ps)) \\ (get_Note (q:qs))) - m
       p2 = get_value_Oct ((get_Oct (p:ps)) \\ (get_Oct (q:qs))) - m
  in
       (m, n1, p2)

 -- Delete the octave in the pitch.
get_Note::[Pitch] -> [Note]
get_Note [] = []
get_Note (Pitch(x1,x2):xs)  = x1 : get_Note xs

 -- Delete the note in the pitch.
get_Oct::[Pitch] -> [Octave]
get_Oct [] = []
get_Oct (Pitch(x1,x2):xs) = x2 : get_Oct xs

 -- Return the pitch value.
get_value_pitch :: [Pitch] -> Int
get_value_pitch [] = 3
get_value_pitch (Pitch x:xs)  = 
 let value_1 = (length (Pitch x:xs))
 in (3 - value_1)

 -- Return the note value.
get_value_Note :: [Note] -> Int
get_value_Note [] = 3
get_value_Note (x:xs)  = 
 let value_1 = (length (x:xs))
 in (3 - value_1)

 -- Return the octave value.
get_value_Oct :: [Octave] -> Int
get_value_Oct [] = 3
get_value_Oct (x:xs)  = 
 let value_1 = (length (x:xs))
 in (3 - value_1)

 -- The value of first chord. We define it as ["A1", "B2", "C3"].
initial_Pitches :: [Pitch]
initial_Pitches = [Pitch (A,1), Pitch(B,2), Pitch(C,3)]

 -- Define the type of gamestate.
type GameState = [[Pitch]]


 -- The total situations of the gamestates.
all_gamestate :: [[Pitch]]
all_gamestate = [[a]++[b]++[c] | a <- pitch, b <- pitch, c <- pitch, (showPitch a)/=(showPitch b)
                       &&(showPitch b)/=(showPitch c)&&(showPitch a)/=(showPitch c)
                      ]
                    where pitch = [(Pitch(a,b))| a<- [A,B,C,D,E,F,G], b<-[1,2,3] ]

 -- The definition of initial guess of the gamestate. It return the value of corresponding pitches
 -- and gamestate.
initialGuess :: ([Pitch],GameState)
initialGuess = (initial_Pitches, all_gamestate)

 -- Input the previous guess and previous gamestate to get the post guess and post gamestate. 
 -- The post_guess is the element of the post_gamestate in the middle.
nextGuess :: ([Pitch],GameState) -> (Int,Int,Int) -> ([Pitch],GameState)
nextGuess (previous_guess, previous_gamestate) (pitch, note, octave) = (post_guess, post_gamestate)
       where post_gamestate =   [pt_guess| pt_guess <- previous_gamestate, feedback previous_guess pt_guess == (pitch, note, octave)]
             post_guess = post_gamestate !! index_value
             index_value = (length post_gamestate) `quot` 2