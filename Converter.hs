
-- to run file :
-- $ bnfc -m grammar.cf
-- $ runghc -iAST/:grammar/ Converter.hs

module Converter where

import Frame
import qualified DataTypes as D
import AbsGrammar
import qualified AbsGrammar as A

main :: IO ()
main = putStrLn "22"

declaration :: A.Declaration -> D.Declaration
declaration (A.DFunc (A.Id name) _ defs) = (D.DFunc name vars expr)
    where expr = convertDefs defs
          vars = getArgs defs

convertDefs :: [A.Def] -> D.Exp
convertDefs = undefined

getArgs :: [A.Def] -> D.Vars
getArgs = undefined
