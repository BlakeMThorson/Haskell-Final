module Main where

import Control.Monad (when)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import Bird.Parser
import Bird.Printer

import Data.Char
import Data.List ( intercalate, isSubsequenceOf )
import GHC.Show (Show)
import Data.List

main :: IO ()
main = do args <- getArgs
          let files = filter ("-p" /=) args
              go | elem "-p" args = goParse
                 | otherwise      = goEval
          when (not (null files)) $
              go  . concat =<< mapM readFile files

--------------------------------------------------------------------------------------------------------------------------------

--MAIN FILES N JUNK

goParse, goEval :: String -> IO ()
goParse s = mapM_ (putStrLn . printSyntax) ((fst . head) (runParser (many parseArgs) s))
-- goEval  s = mapM_ putStrLn (words (runEvals [] ((fst . head) (runParser (many parseArgs) s))))
goEval  s = mapM_ putStrLn (words (handleSyntax ((fst . head) (runParser (many parseArgs) s))))
-- goEval  s = putStrLn (runEvals [] ((fst . head) (runParser (many parseArgs) s)))

--------------------------------------------------------------------------------------------------------------------------------

--AST definitions for terms and their types
data Expr = Atom String | Variable String | Strng String | Compound Expr [Expr] | Nat Expr | List Expr deriving (Show, Eq)
data Syntax = Statement [Expr] |  Query [Expr] | Rule Expr [Expr] | Comment () deriving (Show, Eq)

--------------------------------------------------------------------------------------------------------------------------------

--TO DO NEXT! -- I don't remember what this actually was for, I can't remember monday or tuesday at all and I left my 
--notebook at work
data Tree = Node String [Tree]
  deriving Show

--------------------------------------------------------------------------------------------------------------------------------

handleSyntax :: [Syntax] -> [Char]
handleSyntax (Query q:qs) = runEvals [] (toQueries (Query q)) ++ handleSyntax qs
handleSyntax s = runEvals [] s

runEvals :: [Syntax] -> [Syntax] -> [Char]
runEvals known [] = ""
runEvals known (x:xs) = snd val ++ " " ++ runEvals (fst val) xs
-- runEvals known (x:xs) = concatMap show (fst val) ++ "\n" ++ runEvals (fst val) xs
    where val = evalSyntax known x

evalSyntax :: [Syntax] -> Syntax -> ([Syntax], String)
evalSyntax known (Query q) = if null vars
                             then if length q > 1
                                  then (known, evalQuerySet known (Query q))
                                  else (known, val)
                             else (known, evaluateVarQuery known (Query q))
    where vars = qHasVars (Query q)
          val = if length (toStatements (Query q) `intersect` known) == length (toStatements (Query q)) -- need to change to permutations
                then "Yes."
                else runLib (Query q) 
evalSyntax known (Statement s) = (checkRules (known ++ toStatements (Statement s)), "")
evalSyntax known (Rule l r) = (checkRules (known ++ [Rule l r]), "")
evalSyntax known (Comment c) = (known, "")

checkRules :: [Syntax] -> [Syntax]
checkRules known = nub (concatMap checkRule known)
    where checkRule (Rule l r) = if length (toStatements (Query r) `intersect` known) == length (toStatements (Query r)) -- need to change to permutations
                                 then known ++ [Statement [l]]
                                 else known
          checkRule a = known

-- >>> checkRules [Statement [Atom "a"], Statement [Atom "b"], Statement [Atom "c"], Rule (Atom "x") [Atom "b", Atom "a"]]
-- [Statement [Atom "a"],Statement [Atom "b"],Statement [Atom "c"],Rule (Atom "x") [Atom "b",Atom "a"],Statement [Atom "x"]]

toStatements :: Syntax -> [Syntax]
toStatements (Query (q:qs)) = Statement [q] : toStatements (Query qs)
toStatements (Statement (s:ss)) = Statement [s] : toStatements (Statement ss)
toStatements q = []

toQueries :: Syntax -> [Syntax]
toQueries (Statement (s:ss)) = Query [s] : toQueries (Statement ss)
toQueries (Query (q:qs)) = Query [q] : toQueries (Query qs)
toQueries s = []

-- Check if query contains variables and return vars
qHasVars :: Syntax -> [Expr]
qHasVars (Query exprs) = nub (concatMap hasVars exprs)

-- Check if compound contains a variable obj and return that object
hasVars :: Expr -> [Expr]
hasVars (Compound name (x:xs)) = hasVars x ++ hasVars (Compound name xs)
hasVars (Variable v) = [Variable v]
hasVars other = []

{-  args:    Atom to replace Variables;
             Target Variable to be replaced;
             Expr to modify
    returns: Expr with target Variable replaced by replacement Atom -}
exprReplaceVar :: Expr -> Expr -> Expr -> Expr
exprReplaceVar (Atom sub) (Variable var) (Atom replace) = Atom replace -- does nothing if Atom
exprReplaceVar (Atom sub) (Variable var) (Variable replace) | var == replace = Atom sub -- replace if target var is found
                                                            | otherwise = Variable replace  -- do nothing if not target var
exprReplaceVar (Atom sub) (Variable var) (Compound name exprs) = Compound name (map (exprReplaceVar (Atom sub) (Variable var)) exprs) -- apply to all items is Compound
-- Compounds
-- exprReplaceVar (Compound name exprs) (Variable var) (Compound name2 exprs2) = Compound name2 (map (exprReplaceVar (Compound name exprs) (Variable var)) exprs2) -- note
-- exprReplaceVar (Compound name exprs) (Variable var) (Variable replace) | var == replace = Compound name exprs
--                                                                        | otherwise = Variable replace 
-- exprReplaceVar (Compound name exprs) (Variable var) replace = replace
-- Strings
exprReplaceVar (Strng sub) (Variable var) (Variable replace) | var == replace = Strng sub -- replace if target var is found
                                                             | otherwise = Variable replace  -- do nothing if not target var
exprReplaceVar (Strng sub) (Variable var) (Compound name exprs) = Compound name (map (exprReplaceVar (Strng sub) (Variable var)) exprs) -- apply to all items is Compound
exprReplaceVar (Strng sub) (Variable var) replace = replace

exprReplaceVar (Atom sub) (Variable v) (Strng replace) = Strng replace
--Nats
exprReplaceVar (Nat sub) (Variable var) (Variable replace) | var == replace = Nat sub
                                                           | otherwise = Variable replace
exprReplaceVar (Nat sub) (Variable var) (Compound name exprs) = Compound name (map (exprReplaceVar (Nat sub) (Variable var)) exprs) -- apply to all items is Compound
exprReplaceVar (Nat sub) (Variable var) replace = replace

exprReplaceVar (List sub) (Variable var) (Variable replace) | var == replace = List sub
                                                           | otherwise = Variable replace
exprReplaceVar (List sub) (Variable var) (Compound name exprs) = Compound name (map (exprReplaceVar (List sub) (Variable var)) exprs) -- apply to all items is Compound
exprReplaceVar (List sub) (Variable var) replace = replace


exprReplaceVar _ (Variable var) replace = replace --var3

-- >>> exprReplaceVar ((List (Compound (Atom "cons") [Atom "a",List (Compound (Atom "cons") [Atom "b",Atom "nil"])]))) (Variable "X") (Variable "X")
-- List (Compound (Atom "cons") [Atom "a",List (Compound (Atom "cons") [Atom "b",Atom "nil"])])


{-  args:   Takes [(Query, [(Var, Atom)])] Where query is what is to be worked on, var is filler, atom is filler;
            Variables to be replaced
            What to replace the variables with
    return: [(Query, [(Var, Atom)])] List of queries paired with Variable-Atom pair representing which Variables were replaced with what Atoms in that query.
            Var-Atom pairs only show updates to vars. Use expandUpdates to get full list. -}
newManyQueries :: [(Syntax, [(Expr, Expr)])] -> [Expr] -> [Expr] -> [(Syntax, [(Expr, Expr)])]
newManyQueries [curr] (Variable v:vs) r = newManyQueries newQ vs r
    where newQ = newQueries curr (Variable v) r
newManyQueries (q:qs) (Variable v:vs) r = newManyQueries newQ vs r
    where newQ = newQueries q (Variable v) r ++ newManyQueries qs [Variable v] r
newManyQueries q [] r = q
newManyQueries q v [] = q
newQueries :: (Syntax, [(Expr, Expr)]) -> Expr -> [Expr] -> [(Syntax, [(Expr, Expr)])]
newQueries (Query expr, curr) (Variable v) (r:rs) = (Query (map (exprReplaceVar r (Variable v)) expr), (curr ++ [(Variable v, r)])) : newQueries (Query expr, []) (Variable v) rs -- can assume query has one expr in it
newQueries (Query expr, _) (Variable v) empty = []
-- support for statements
newQueries (Statement expr, curr) (Variable v) (r:rs) = (Statement (map (exprReplaceVar r (Variable v)) expr), (curr ++ [(Variable v, r)])) : newQueries (Statement expr, []) (Variable v) rs -- can assume query has one expr in it
newQueries (Statement expr, _) (Variable v) empty = []


{-  args:   List of Var-Atom pairs from newManyQueries
    return: Full list with missing variable definitions fleshed out
-}
expandUpdates :: [[(Expr, Expr)]] -> [[(Expr, Expr)]]
expandUpdates a = moreUpdates (head a) a
moreUpdates :: [(Expr, Expr)] -> [[(Expr, Expr)]] -> [[(Expr, Expr)]]
moreUpdates curr (u:us) = new : moreUpdates new us
    where new = updates curr u
moreUpdates curr [] = []
updates :: [(Expr, Expr)] -> [(Expr, Expr)] -> [(Expr, Expr)]
updates curr (u:us) = updates new us
    where new = update curr u
updates curr [] = curr
update :: [(Expr, Expr)] -> (Expr, Expr) -> [(Expr, Expr)]
update (curr:currs) new | fst new == fst curr = new : update currs new
                        | otherwise = curr : update currs new
update [] new = []


runVarQueries :: [Syntax] -> [Syntax] -> String
runVarQueries known (Query q:qs) = snd (evalSyntax known (Query q)) ++ runVarQueries known qs
runVarQueries known empty = ""

evaluateVarQuery :: [Syntax] -> Syntax -> String
evaluateVarQuery known (Query q) = printVarCheck (checkVarQueries known (fixNewQueries (newManyQueries [(Query q, [])] (qHasVars (Query q)) (getVarsKnown known))))

fixVarAtoms :: [(Syntax, [(Expr, Expr)])] -> [[(Expr, Expr)]] -> [(Syntax, [(Expr, Expr)])]
fixVarAtoms (va:vas) (r:rs) = replace va r : fixVarAtoms vas rs
    where replace :: (Syntax, [(Expr, Expr)]) -> [(Expr, Expr)] -> (Syntax, [(Expr, Expr)])
          replace (q, arr) new = (q, new)
fixVarAtoms v r = []

fixNewQueries :: [(Syntax, [(Expr, Expr)])] -> [(Syntax, [(Expr, Expr)])]
fixNewQueries a = fixVarAtoms a (expandUpdates (map snd a))


checkVarQueries :: [Syntax] -> [(Syntax, [(Expr, Expr)])] -> [[String]]
checkVarQueries known (s:ss) = checkVarQuery known s : checkVarQueries known ss
checkVarQueries known [] = [[]]
checkVarQuery :: [Syntax] -> (Syntax, [(Expr, Expr)]) -> [String]
checkVarQuery known (Query q, va) = if check
                                    then printVarAtoms va
                                    else []
    where check = length (toStatements (Query q) `intersect` known) == length (toStatements (Query q))
checkVarQuery [] _ = []

printVarCheck :: [[String]] -> String
printVarCheck ss = if null (filter (not . null) ss)
                   then "No."
                   else "Yes:\n" ++ printVarCheck'' (filter (not . null) ss) ++ "."
    where printVarCheck'' (s:ss) = if check
                                   then printVarCheck' s
                                   else printVarCheck' s ++ ";\n" ++ printVarCheck'' filtered
            where filtered = filter (not . null) ss
                  check = null ss
          printVarCheck'' [] = ""
          printVarCheck' = intercalate ",\n"

evalQuerySet :: [Syntax] -> Syntax -> String
evalQuerySet known (Query q) = if elem "No." (map snd (map (evalSyntax known) (qToQs (Query q))))
                               then "No."
                               else "Yes."

qToQs :: Syntax -> [Syntax]
qToQs (Query exprs) = map Query (qToCs exprs)

qToCs :: [Expr] -> [[Expr]]
qToCs (e:es) = [e] : qToCs es
-- qToCs ((Compound name exprs):es) = [Compound name exprs] : qToCs es
-- qToCs (e:es) = [e] : qToCs es
qToCs [] = []


printVarAtoms :: [(Expr, Expr)] -> [String]
printVarAtoms ((Variable v, expr):vas) = (v ++ "=" ++ printAST expr) : printVarAtoms vas
printVarAtoms _ = []




--------------------------------------------------------------------------------------------------------------------------------

-- PRINTS

printAST :: Expr -> String
printAST (Atom x) = x
printAST (Variable x) = x
printAST (Strng x) = '"' : x ++ ['"']
printAST (Compound x xs) = printAST x ++ "(" ++ intercalate "," (map printAST xs) ++ ")"
    where printNum [] = ""
          printNum (x:xs) = "(" ++ x ++ printNum xs ++ ")"
printAST (Nat n) = printAST n
printAST (List n) = printAST n

printSyntax :: Syntax -> String
printSyntax (Query xs) = intercalate "," (map printAST xs) ++ "?"
printSyntax (Statement xs) = intercalate ", " (map printAST xs) ++ "."
printSyntax (Rule left right) = printAST left ++ " :- " ++ intercalate ", " (map printAST right) ++ "."
printSyntax (Comment xs) = ""

--------------------------------------------------------------------------------------------------------------------------------

--MAIN PARSERS

--top level
parseArgs :: Parser Syntax
parseArgs = grabComment <|> statement <|> query <|>  rule

--inside / lower parser 
parseTerm :: Parser Expr
parseTerm =  number <|> variable <|> stringp <|> atom <|> list <|> listBar

--------------------------------------------------------------------------------------------------------------------------------

--SIMPER PARSERS

stringp :: Parser Expr
stringp = do c1 <- token (satisfy (=='"'))
             cs <- many (satisfy (/= '"'))
             c1 <- token (satisfy (=='"'))
             return (Strng cs)

variable :: Parser Expr
variable = do
    x <- token upper
    xs <- many lower
    return(Variable (x:xs))

number :: Parser Expr
number = do x <- token(many1 digit)
            return(convertNum x)
    where convertNum arr = addComp n (Atom "zero")
           where n = read (concatMap show arr) :: Int
                 addComp 0 curr = curr
                 addComp n curr = addComp (n - 1) (Nat (Compound (Atom "succ") [curr]))

-- >>> runParser number "2"
-- [(Nat (Compound (Atom "succ") [Nat (Compound (Atom "succ") [Atom "zero"])]),"")]

grabComment :: Parser Syntax
grabComment = do
    x <- token(satisfy (== '%'))
    xs <- token (many (satisfy(/= '\n')))
    return (Comment ())

--------------------------------------------------------------------------------------------------------------------------------

-- VAR HANDLING (WIP)



-- >>> subVar (Atom "hello") (Variable "hi") (Compound (Atom "hi") [Atom "hi" ,Compound (Atom "name") [Variable "hi", Atom "hi", Atom "yo", Atom "hi"]])
-- Compound (Atom "hi") [Atom "hi",Compound (Atom "name") [Atom "hello",Atom "hi",Atom "yo",Atom "hi"]]

getVarsKnown :: [Syntax] -> [Expr]
getVarsKnown known = nub (concatMap getVarsStmt known)

getVarsStmt :: Syntax -> [Expr]
getVarsStmt (Statement (x:xs)) = getVarsComp x ++ getVarsStmt (Statement xs)
getVarsStmt empty = []

getVarsComp :: Expr -> [Expr]
getVarsComp (Compound _ [Atom a]) = [Atom a]
getVarsComp (Compound _ [Nat n]) = [Nat n]
getVarsComp (Compound _ [Atom a, List n]) = List n : getVarsComp n ++ indElems (List n)
getVarsComp (List (Compound name [Atom a, List n])) = Atom a : List (Compound name [Atom a, List n]) : getVarsComp (List n)
getVarsComp (List (Compound _ [Atom a, Atom b])) = [Atom a]
getVarsComp (Compound _ (x:xs)) = getVarsComp x ++ getVarsStmt (Statement xs)
getVarsComp (Compound _ empty) = empty
getVarsComp atoms = [atoms]

indElems :: Expr -> [Expr]
indElems (List (Compound _ [Atom a, List n])) = Atom a : indElems (List n)
indElems (List (Compound _ [Atom a, Atom b])) = [Atom a]

{-
getVarsComp :: Expr -> [Expr]
getVarsComp (Compound _ [Atom a]) = [Atom a]
getVarsComp (Compound _ [Nat n]) = [Nat n]
getVarsComp (Compound name [Atom a, List n]) = Compound name [Atom a, List n] : getVarsComp n
getVarsComp (List (Compound name [Atom a, List n])) = Compound name [Atom a, List n] : getVarsComp (List n)
getVarsComp (List (Compound _ [Atom a, Atom b])) = [Atom b]
getVarsComp (Compound _ (x:xs)) = getVarsComp x ++ getVarsStmt (Statement xs)
getVarsComp (Compound _ empty) = empty
getVarsComp atoms = [atoms]
-}

-- >>> getVarsKnown [((fst . head) (runParser statement "p([a,b,c])."))]
-- [Atom "a",List (Compound (Atom "cons") [Atom "a",List (Compound (Atom "cons") [Atom "b",List (Compound (Atom "cons") [Atom "c",Atom "nil"])])]),Atom "b",List (Compound (Atom "cons") [Atom "b",List (Compound (Atom "cons") [Atom "c",Atom "nil"])]),Atom "c"]

-- >>> (getVarsKnown [((fst . head) (runParser statement "[a,b, c]."))])
-- [Atom "a",Compound (Atom "cons") [Atom "a",List (Compound (Atom "cons") [Atom "b",List (Compound (Atom "cons") [Atom "c",Atom "nil"])])],Atom "b",Compound (Atom "cons") [Atom "b",List (Compound (Atom "cons") [Atom "c",Atom "nil"])],Atom "c"]

-- >>> ((fst . head) (runParser statement "p([a,b])."))
-- Statement [Compound (Atom "p") [List (Compound (Atom "cons") [Atom "a",List (Compound (Atom "cons") [Atom "b",Atom "nil"])])]]

-- >>> indElems (List (Compound (Atom "cons") [Atom "a",List (Compound (Atom "cons") [Atom "b",Atom "nil"])]))
-- [Atom "a",Atom "b"]
--------------------------------------------------------------------------------------------------------------------------------

atom :: Parser Expr
atom = do
    x <- token lower
    xs <- many(satisfy isLetter)
    return (Atom (x:xs))

compound :: Parser Expr
compound =
    do
    x <- token parseTerm
    rest <- parens (sepBy (symbol ",") compound)
    if      isNat  x then return (Nat  (Compound x rest))
    else if isList x then return (List (Compound x rest))
    else    return                     (Compound x rest)
 <|>
    do parseTerm

-- >>> runParser query "p(cons(a,cons(b,c)))?"
-- [(Query [Compound (Atom "p") [List (Compound (Atom "cons") [Atom "a",List (Compound (Atom "cons") [Atom "b",Atom "c"])])]],"")]

isNat :: Expr -> Bool
isNat (Atom a) = a == "succ" || a == "zero"
isNat _ = False

isList :: Expr -> Bool
isList (Atom a) = a == "cons" || a == "nil"
isList _ = False

statement  :: Parser Syntax
statement  = do
            ys <- sepBy (symbol ",") compound
            symbol "."
            return (Statement ys)

query  :: Parser Syntax
query  = do
            ys <- sepBy (symbol ",") compound
            symbol "?"
            return (Query ys)

rule  :: Parser Syntax
rule = do
        -- xs <- sepBy1 (token(string ":-")) compound
        left <- compound
        symbol ":-"
        right <- sepBy (symbol ",") compound
        symbol "."
        return (Rule left right)

list :: Parser Expr
list = do rest <- brackets (sepBy (symbol ",") parseTerm)
          return (convertList (reverse rest) (Atom "nil"))

convertList :: [Expr] -> Expr -> Expr
convertList [] curr = curr
convertList (x:xs) curr = convertList xs (List (Compound (Atom "cons") [x, curr]))

listBar :: Parser Expr
listBar = do symbol "["
             content <- sepBy (symbol ",") parseTerm
             symbol "|"
             barContent <- parseTerm
             symbol "]"
             return (convertList (reverse content) barContent)

runLib :: Syntax -> String
runLib (Query [Compound (Atom name) [a, b, c]]) | name == "addn"    = convertBool (addn    a b c)
                                                | name == "multn"   = convertBool (multn   a b c)
                                                | name == "addi"    = convertBool (addi    a b c)
                                                | name == "multi"   = convertBool (multi   a b c)
runLib (Query [Compound (Atom name) [a, b]])    | name == "eqi"     = convertBool (eqi     a b)
                                                | name == "inverse" = convertBool (inverse a b)
runLib _ = "No."

addn, multn, addi, multi :: Expr -> Expr -> Expr -> Bool
addn   a b c = toNum a + toNum b == toNum c
multn  a b c = toNum a * toNum b == toNum c
addi   (Compound _ [a, b]) (Compound _ [c, d]) (Compound _ [e, f]) = encodeInt a b + encodeInt c d == encodeInt e f
multi  (Compound _ [a, b]) (Compound _ [c, d]) (Compound _ [e, f]) = encodeInt a b * encodeInt c d == encodeInt e f
-- append ()

eqi, inverse :: Expr -> Expr -> Bool
eqi     (Compound _ [a, b]) (Compound _ [c, d]) = encodeInt a b == encodeInt c d
inverse (Compound _ [a, b]) (Compound _ [c, d]) = encodeInt a b == negate (encodeInt c d)

encodeInt :: Expr -> Expr -> Int
encodeInt a b = toNum a - toNum b

-- >>> (fst . head) (runParser parseTerm "2")
-- Nat (Compound (Atom "succ") [Nat (Compound (Atom "succ") [Atom "zero"])])


-- >>> (fst . head) (runParser query "addn(zero,2,2)?")
-- Query [Compound (Atom "addn") [Atom "zero",Nat (Compound (Atom "succ") [Nat (Compound (Atom "succ") [Atom "zero"])]),Nat (Compound (Atom "succ") [Nat (Compound (Atom "succ") [Atom "zero"])])]]

-- >>> runLib ((fst . head) (runParser query "addi(1,1,3)?"))
-- "No."

-- >>> evalSyntax [] ((fst . head) (runParser query "addn(0,2,1)?"))
-- ([],"No.")

toNum :: Expr -> Int
toNum (Nat (Compound name [rest])) = 1 + toNum rest
toNum (Atom "zero") = 0


convertBool :: Bool -> String
convertBool True = "Yes."
convertBool False = "No."
