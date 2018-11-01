module GrammarDef where

import Data.List

class Show a => Pretty a where
  pretty :: a -> String
  pretty = show

data Part = Epsilon | T String | NT String deriving (Eq, Ord, Show)
-- rather than overloading the Show type class (which can make debugging harder) we ass a function pretty.

instance Pretty Part where
  pretty Epsilon = "#"
  pretty (T s) = s
  pretty (NT s) = s

is_nonterminal Epsilon = False
is_nonterminal (T _) = False
is_nonterminal (NT _) = True

name_of Epsilon = ""
name_of (T s) = s
name_of (NT s) = s

-- replace character c with string s in a list.  Used to help pretty-print grammars.
replace (c,s) [] = []
replace (c,s) (x:xs) = if c == x then s ++ replace (c,s) xs else (x: replace (c,s) xs)
decomma s = replace (',' , " ") (unwrap 1 s)

-- unwrap: deletes j elements from either end of a list.
--         used to pretty-print grammars.
unwrap j s = let k = length s in drop j (take (k-j) s)

data Production = P String [[Part]] deriving (Eq,Show)

dequote  = replace ('\"', "")
delist sep s =  unwrap 1 (replace (',', sep) (dequote s))

-- rather than overloading the Show type class (which can make debugging harder) we ass a function pretty.
instance Pretty Production where
  pretty (P name parts) =
    let parts' = delist " | " $ dequote $ show (map (\ps -> delist " " (dequote (show (map pretty ps)))) parts) in
        name ++ " => " ++ parts'

production_name (P name _) = name
names_in_prod (P name prods) = nub $ concatMap (map name_of) prods

data Grammar = G String [Production] deriving Show
-- rather than overloading the Show type class (which can make debugging harder) we ass a function pretty.

instance Pretty Grammar where
  pretty (G name productions) = name ++ "{\n" ++ prods ++ "}\n"
               where prods = (delist "\n" $ dequote $ show $ map pretty productions) ++ "\n"
