import Lexer
import System.Environment

data Astree t = AddExp (Astree t, Astree t)|
             MulExp (Astree t, Astree t)|
             SubExp (Astree t, Astree t)|
             IfExp (Astree t)|
             ThenExp (Astree t)|
             ElseExp (Astree t)|
             IfThenElseExp (Astree t, Astree t, Astree t)|
             LetInExp (Astree t, Astree t)|
             LetExp [Astree t]|
             InExp (Astree t)|
             BinExp (Astree t)|
             BoolExp (Astree t)|
             Num t|
             Const t|
             Id t|
             AndExp (Astree t, Astree t)|
             OrExp (Astree t, Astree t)|
             XorExp (Astree t, Astree t)|
             NotExp (Astree t)|
             LessThanExp (Astree t, Astree t)|
             GreaterThanExp (Astree t, Astree t)|
             NegateExp (Astree t)|
             ImpliesExp (Astree t, Astree t)|
             EqualityExp (Astree t, Astree t)|
             AssignExp (Astree t, Astree t) |
             EmptyT
             deriving (Show, Eq)


parser str = myAlexScanTokens str

isEmpty [] = True
isEmpty (x: xs) = False

doNothing = undefined

------ Follow -----------------------
-- LESSTHAN, GREATERTHAN, IMPLIES, ), *, +, -, AND, OR, XOR, BOOLEQUALS, ID, 
-- IN, then,EOF,else, fi,end

isFollowC' curToken = isLtToken curToken
                      || isGtToken curToken
                      || isImpliesToken curToken
                      || isRparenToken curToken
                      || isTimesToken curToken
                      || isPlusToken curToken
                      || isMinusToken curToken
                      || isAndToken curToken
                      || isOrToken curToken
                      || isXorToken curToken
                      || isBEToken curToken
                      || isIdToken curToken
                      || isInToken curToken
                      || isthenToken curToken
                      || isEofToken curToken
                      || iselseToken curToken
                      || isfiToken curToken
                      || isEndToken curToken

isFollowD' curToken = isFollowC' curToken
isFollowF' curToken = isFollowC' curToken
isFollowH' curToken = isFollowC' curToken
isFollowG' curToken = isFollowC' curToken
isFollowK' curToken = isInToken curToken 


---------- p Start Symbol ------------

isBEToken curToken = (isConstToken curToken) 
                     || (isIdToken curToken)
                     || (isNegateToken curToken)
                     || (isNotToken curToken)
                     || (isNumToken curToken)
                     || (isLparenToken curToken)

--- pAux Start

pAuxStart ls = 
    let
        (leftTree, rem1) = pS ls
        (rightTree, rem2) = pZ rem1
    in 
        if length rem2 == 0
        then leftTree
        else doNothing

pZ [] = error $ "Syntax Error: Missing 'EOF' token"
pZ ls 
    | (length ls == 1) && (isEofToken (head ls)) = (EmptyT, [])
    | otherwise = error $ "Syntax Error: " ++ show (head ls) ++ "Production Rule : Z -> EOF"
     


 
pS ls 
    | isEmpty ls = error $ "Syntax Error: " ++ "Production Rule : Start -> LetExp Z| IfExp Z| BoolExp Z"
    | letToken = pLet ls
    | ifToken = pITE ls
    | boolExpToken = pB ls
    | otherwise = error $ "Syntax Error: " ++ tokenPosn (head ls) ++ "Production Rule : Start -> LetExp Z| IfExp Z| BoolExp Z"
           
    where
        curToken = head ls
        letToken = (isLetToken curToken)
        ifToken = (isIfToken curToken)
        boolExpToken = (isBEToken curToken)


------- pBool -----------

pB ls 
     | isEmpty ls = error $ "Syntax Error: " ++ "Production Rule : <B -> C C'>"
     | isEmpty rem1 = (leftTree, rem1)
     | otherwise = case (first curToken) of
                   "implies" -> (ImpliesExp (leftTree, rightTree), rem2)
                   otherwise -> (leftTree, rem2)
     where
          (leftTree, rem1) = pC ls
          curToken = head rem1
          (rightTree, rem2) = pC' rem1

pC ls 
     | isEmpty ls = error $ "Syntax Error: " ++ "Production Rule : <C  -> D D'>"
     | isEmpty rem1 = (leftTree, rem1)
     | otherwise = case (first curToken) of
                    ">" -> (BoolExp(GreaterThanExp (leftTree, rightTree)), rem2)
                    "<" -> (BoolExp(LessThanExp (leftTree, rightTree)), rem2)
                    otherwise -> (leftTree, rem2)
     where
          (leftTree, rem1) = pD ls
          curToken = head rem1
          (rightTree, rem2) = pD' rem1

pC' [] = (EmptyT, [])
pC' ls 
    | isImpliesToken curToken = pB remTokens
    | isFollowC' curToken = (EmptyT, ls)
    | otherwise = error $ "Syntax Error: " ++ tokenPosn (head ls) ++ "Production Rule :C' -> IMPLIES B | Epsilon"
    where
         curToken = head ls
         remTokens = tail ls

pD ls 
     | isEmpty ls = error $ "Syntax Error: " ++ "Production Rule : <D -> H H'>"
     | isEmpty rem1 = (leftTree, rem1) 
     | otherwise = case (first curToken) of
                   "&&" -> (BoolExp(AndExp (leftTree, rightTree)), rem2)
                   "||" -> (BoolExp(OrExp (leftTree, rightTree)), rem2)
                   "xor" -> (BoolExp(XorExp (leftTree, rightTree)), rem2)
                   "==" -> (BoolExp( EqualityExp (leftTree, rightTree)), rem2)
                   otherwise -> (leftTree, rem2)
     where
          (leftTree, rem1) = pH ls
          curToken = head rem1
          (rightTree, rem2) = pH' rem1

pD' [] = (EmptyT, [])
pD' ls 
     | (isLtToken curToken) || (isGtToken curToken) = pC remTokens
     | isFollowD' curToken = (EmptyT, ls)
     | otherwise = error $ "Syntax Error: " ++ tokenPosn (head ls) ++ "Production Rule : D' -> LESSTHAN C | GREATERTHAN C | Epsilon"
     where
          curToken = head ls
          remTokens = tail ls

pH ls 
     | length ls == 0 = error $ "Syntax Error: " ++ "Production Rule : <H -> CONST | E>"
     | isConstToken curToken = (Const (first (curToken)), remTokens)
     | otherwise = pE ls
     where
          curToken = head ls
          remTokens = tail ls

pH' [] = (EmptyT, [])
pH' ls 
     | (isOrToken curToken) || (isAndToken curToken) || (isXorToken curToken) || (isEqualsToken curToken) = pD remTokens
     | isFollowH' curToken = (EmptyT, ls)
     | otherwise = error $ "Syntax Error: " ++ tokenPosn (head ls) ++ "Production Rule : H' -> AND D| OR D| XOR D| BOOLEQUALS D | Epsilon"
     where
          curToken = head ls
          remTokens = tail ls
--pE :: [Token] -> (Astree t, [Token]) 

pE [] = error $ "Syntax Error: " ++ "Production Rule : <E -> F F'>"
pE ls
    |  isEmpty rem1 = (leftTree, rem1)
    |  otherwise = case (first curToken) of
                    "+" -> (BinExp (AddExp (leftTree, rightTree)), rem2)
                    "-" -> (BinExp (SubExp (leftTree, rightTree)), rem2)
                    otherwise -> (leftTree, rem2)
    where
         curToken = head rem1
         (leftTree, rem1) = pF ls
         (rightTree, rem2) = pF' rem1


pF ls
    | isEmpty ls = error $ "Syntax Error: " ++ "Production Rule : <F -> G G'>"
    | isEmpty rem1 = (leftTree, rem1)
    | otherwise = case (first curToken) of
                   "*" -> (BinExp (MulExp (leftTree, rightTree)), rem2)
                   otherwise -> (leftTree, rem2)
    where
        (leftTree, rem1) = pG ls
        curToken = head rem1 
        (rightTree, rem2) = pG' rem1

pF' [] = (EmptyT, [])
pF' ls
    | (isPlusToken curToken) || (isMinusToken curToken) = pE remTokens
    | isFollowF' curToken = (EmptyT, ls)
    | otherwise = error $ "Syntax Error: " ++ tokenPosn (head ls) ++ "Production Rule : F' -> + E | - E | Epsilon"
    where
        curToken = head ls
        remTokens = tail ls


pG ls
    | isEmpty ls = error $ "Syntax Error: " ++ "Production Rule : <G -> ID | Num | ~E | (B) | NOT B>"
    | isIdToken curToken = (Id (first curToken), remTokens) 
    | isNumToken curToken = (Num (first curToken), remTokens)
    | isNegateToken curToken = (NegateExp tree, l1)
    | isNotToken curToken = (NotExp tree, l1)
    | isLparenToken curToken = case isEmpty l1 of
                                True -> error "Missing token RPAREN ')' "
                                False -> case isRparenToken (head l1) of
                                         True -> (tree, tail l1)
                                         False -> error $ "Syntax Error: " ++ tokenPosn (head l1) ++ "Production Rule : G -> ID | Num | ~E | (B) | NOT B"
    | otherwise = error $ "Syntax Error: " ++ tokenPosn (head ls) ++ "Production Rule : <G -> ID | Num | ~E | (B) | NOT B>"
    where
        curToken = head ls
        remTokens = consumeFirst ls
        (tree, l1) = pB (remTokens) -- change

pG' [] = (EmptyT, [])
pG' ls
    | isTimesToken curToken = pF remTokens
    | isFollowG' curToken = (EmptyT, ls)
    | otherwise = error $ "Syntax Error: " ++ tokenPosn (head ls) ++ "Production Rule : G' -> * F | Epsilon"
    where
        curToken = head ls
        remTokens = consumeFirst ls

----- boolean expr.


makeLet (AssignExp (a, b)) (LetExp d) = LetExp ((AssignExp (a, b)) : d)

pLet ls
    | isLetToken curToken = case (isEmpty rem1) of
                                True -> error $ "Syntax Error : Missing Token 'in' "
                                False -> case (isInToken (head rem1)) of
                                            True -> case (isEmpty rem2) of
                                                        True -> error $ "Syntax Error: Missing Token 'end' after in : Production Rule : LetExp -> LET K IN S END"
                                                        False -> case (isEndToken (head rem2)) of
                                                                    True -> (LetInExp (leftTree, InExp (rightTree)), tail rem2)
                                                                    False -> error $ "Syntax Error: " ++ tokenPosn (head rem2) ++ "Production Rule : LetExp -> LET K IN Start END"
                                            False -> error $ "Syntax Error: " ++ tokenPosn (head rem1) ++ "Production Rule : LetExp -> LET K IN Start END"

    where 
        curToken = head ls
        (leftTree, rem1) = pK (tail ls)
        (rightTree, rem2) = pS (tail rem1)


pK ls 
    | isIdToken curToken  = case (isEmpty afterId) of
                            True -> error $ "Syntax Error: Expected Token '=' "
                            False -> case (isEqualToken (head afterId)) of
                                         True -> case (tree2 == EmptyT) of
                                                 True -> (LetExp [AssignExp((Id(first (head ls))), tree1)], rem2)
                                                 False -> ((makeLet (AssignExp((Id(first (head ls))), tree1)) tree2), rem2)
                                         False -> error $ "Syntax Error: " ++ tokenPosn (head afterId) ++ "Production Rule : K -> (ID = B) K'"  
    | otherwise = error $ "Syntax Error: " ++ tokenPosn (head ls) ++ "Production Rule : K -> (ID = B) K'"
    where
        curToken = head ls
        afterId = tail ls
        afterEq = tail afterId
        (tree1, rem1) = pB afterEq
        (tree2, rem2) = pK' rem1


pK' [] = (EmptyT, [])
pK' ls 
    | isIdToken curToken = pK ls
    | isFollowK' curToken = (EmptyT, ls)
    | otherwise = error $ "Syntax Error: " ++ tokenPosn (head ls) ++ "Production Rule : K' -> K | Epsilon"
    where
        curToken = head ls
-------- If Expression --------------

pITE ls 
    | isIfToken curToken = case isEmpty rem1 of
                           True -> error $ "missing then, else, fi"
                           False -> case isthenToken (head rem1) of
                                    True -> case isEmpty rem2 of
                                            True -> error $ "missing else after then"
                                            False -> case iselseToken (head rem2) of
                                                     True -> case isEmpty rem3 of
                                                             True -> error $ "missing fi after else"
                                                             False -> case isfiToken (head rem3) of
                                                                      True -> (IfThenElseExp (IfExp ifTree, ThenExp thenTree, ElseExp elseTree), remTokens4)
                                                                      False -> error $ "Syntax Error: " ++ tokenPosn (head rem3) ++ "Production Rule : IfExp -> If E then S else S fi"
                                                     False -> error $ "Syntax Error: " ++ tokenPosn (head rem2) ++ "Production Rule : IfExp -> If E then S else S fi"
                                    False -> error $ "Syntax Error: " ++ tokenPosn (head rem1) ++ "Production Rule : IfExp -> If E then S else S fi"
    | otherwise = (EmptyT, ls)
    where
        curToken = head ls
        remTokens = consumeFirst ls
        (ifTree, rem1) = pB remTokens
        remTokens2 = consumeFirst rem1
        (thenTree, rem2) = pS remTokens2
        remTokens3 = consumeFirst rem2
        (elseTree, rem3) = pS remTokens3
        remTokens4 = consumeFirst rem3


main = do
     get <- getArgs
     let testFile = head (get :: [String])
     contents <- readFile testFile
     let input = contents::String
     let tokens = myAlexScanTokens input
     putStrLn "Lexer Ouput: "
     print $ tokens
     let abstractSyntaxTree = pAuxStart tokens
     putStrLn "Parser Ouput: "
     print $ abstractSyntaxTree