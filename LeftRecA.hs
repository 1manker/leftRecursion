--Lucas Manker

module LeftRecA where

import Data.List
import GrammarDef



{- Here's an example grammar that has left recursion
   EXP, NUMBER and DIGIT are non-terminal symbols and the terminal symbols are
   {'(',')','+','-','0','1','2','3','4','5','6','7','8','9'}

     EXP -> EXP '+' EXP | '(' '-' EXP ')' | NUMBER
     NUMBER -> NUMBER DIGIT | DIGIT
     DIGIT -> '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'  

In the datattypes we've defined, this grammar would be represented as follows:

   Grammar[Production "EXP" [[NT "EXP"', T "+", NT exp], [T "(", T "-", NT "EXP", T ")"], [NT number]],
           Production "NUMBER" [[NT "NUMBER", NT "DIGIT"], [NT "DIGIT"]],
           Production "DIGIT" [[T "0"],[T "1"],[T "2"],[T "3"],[T "4"],[T "5"],[T "6"],[T "7"],[T "8"],[T "9"]]]
  
-}

{- we can give a seperate defintion for each production -}

exp_production = P "EXP" [[NT "EXP", T "+", NT "EXP"], [T "(", T "-", NT "EXP", T ")"], [NT "NUMBER"]]

number_production = P "NUMBER" [[NT "NUMBER", NT "DIGIT"], [NT "DIGIT"]]

digit_production = P "DIGIT" 
                     [[T "0"],[T "1"],[T "2"],[T "3"],[T "4"],[T "5"],[T "6"],[T "7"],[T "8"],[T "9"]]

exp_grammar =  G "Exp" [exp_production, number_production, digit_production]




-- is_left_recursive: returns true if a Production is left recursive.

is_left_recursive (P name prods) = some (\ps -> take 1 ps == [NT name]) prods
       where some p [] = False
             some p (x:xs) = p x || some p xs


-- thin gets rid of empty productions
thin (P name prods) = P name (filter (not . null) prods)



-- Some Assumptions about Productions -
--    Each list of parts in a production is unique - i.e. they are all different.
-- example : let p1 = P "E" [[T "Nat"],[NT "E", T "+", NT "E"], [NT "E", T "*", NT "E"]]

names_of_grammar (G _ productions) = nub $ concatMap names_in_prod productions


-- remove_all: takes a list of elements to be removed from the second list.
remove_all  xs [] = []
remove_all xs (y:ys) = if y `elem` xs then (remove_all xs ys) else (y : remove_all xs ys)


-- cleanup: This function deletes empty Productions and deletes all
--    references to them, also gets rid of right hand sides that are empty. 
cleanup (G name productions) =
   let productions' = map thin productions in
   let (empty, nonempty)  = partition (\(P name prods) -> null prods) productions' in
   let bad_nonterminals = map (NT . production_name) empty in
   let fixed = map (\ (P name prods) -> thin (P name ( nub (map (remove_all bad_nonterminals) prods)))) nonempty in
      (G name fixed)


-- new_name: used to get a string not included in "used_names" you get to suggest a name.

new_name proposed_name used_names =
   if proposed_name `elem` used_names then
      new_name (proposed_name ++ "'") used_names
   else
      proposed_name

-- interleave: interleaves elements from two lists.  Useful for crating the A and A' production sets.
interleave [] _ = []
interleave _ [] = []
interleave (x:xs) (y:ys) = x : y : (interleave xs ys)

process_a_i already_processed_prods (P a_i rsides) =
                 foldr foreach_a_j (P a_i rsides) already_processed_prods

foreach_a_j  (P a_j rsides_j) (P a_i rsides_i)  =
    let (left_corners, others) =
           partition (\parts -> (take 1 parts) == [NT a_j]) rsides_i in
    let left_corners' = map (\ (_ : alpha) -> (map (\beta -> beta ++ alpha) rsides_j)) left_corners in
            (P a_i (concat left_corners' ++ others))

-- Some assumptions about Grammars
--    Each non-terminal is associated with exactly one production

eliminate_left_recursion g @ (G gname productions) =
    let used_names = names_of_grammar g in
    let process_productions already_processed to_be_processed used_names =
          if (null to_be_processed) then
              already_processed
          else
              let (P name parts') = process_a_i already_processed (head to_be_processed) in
              let (p', new_prods, used_names') =
                              eliminate_direct_left_recursion (P name parts') used_names in
              let (already_processed', to_be_processed') =
                        if is_left_recursive p' then
                             (already_processed, [p'] ++ new_prods ++ to_be_processed)
                        else
                             (already_processed ++ [p'], new_prods ++ (drop 1 to_be_processed)) in
                  if null to_be_processed' then
                     already_processed'
                  else
                     process_productions already_processed' to_be_processed' used_names' in
    let productions' = process_productions [] productions used_names in                
      cleanup (G gname productions')



eliminate_direct_left_recursion :: Production -> [String] -> (Production, [Production], [[Char]])
-- eliminate_direct_left_recursion: this is the function you must implement.  The first argument is the
--  production to eliminate direct left recurson from.  The second is a list fo names used so far.  Your
-- function should return a triple (p,ps',used_names') where p is the modified production that was passed in.
-- ps' is hte list of auxlilary productions built by your algorithm and used_names' is the list of used names
-- that were passed in together with any new names you may have created.  I have inserted a dummy body so the
-- file type checks.  Implement the algorithm described on page 260 of the paper "Removing Left Recursion from
-- Context-Free Grammars" by Robert Moore, included as a pdf with the assignment.  The simpler algorithm that
-- introduces Epsilon complicates the removal of indirect left recursion.


eliminate_direct_left_recursion (p @ (P name parts)) used_names =
  {- dummy code - put your code here -}     (p,[],used_names)




