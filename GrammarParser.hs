module GrammarParser where

{-

Here's a simple grammar for representing grammars.

G -> "Grammar" Identifier "{" P+ "}"
P ->  Identifier "=" SymSeq "|" SymSeq    |    SymSeq
SymSeq ->  S "," SymSeq | S
S ->  NT | T
NT -> Identifier
T ->   identifier 
Where:
    Identifier - is an identifier starting with an uppercase letter, 
    identifier - is an identifier starting with a lower case letter.

-}

import Parser
import GrammarDef

grammarP :: Parser Grammar
grammarP = do name <- nameP
              prods <- productionsP
              return (G name prods)

ntP = do {i <- token identupper; return (NT i)}
tP = do {i <- identifier;  return (T i)}

symP :: Parser Part
symP  = (ntP +++ tP)

symseqP :: Parser [Part]
symseqP = some symP



nameP = token identupper

-- productionsP :: Parser [Production]
productionsP = do symbol "{"
                  p <- productionP
                  more <- many (do symbol ","
                                   productionP)
                  symbol "}"
                  return (p:more)


productionP :: Parser Production
productionP = do name <- nameP
                 symbol "->"
                 rhs <- symseqP
                 more <- many (do symbol "|"
                                  symseqP)
                 return (P name (rhs : more))



