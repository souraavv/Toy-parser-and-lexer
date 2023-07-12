{
module Lexer where
}

%wrapper "posn"
$digit = 0-9 
$alpha = [a-zA-Z] 

tokens :-
$white+                         ;
"--".*                          ;
let                             {\p s-> LET p s}
in                              {\p s-> IN p s}
[\+]                            {\p s -> PLUS p s}
[\-]                            {\p s -> MINUS p s}
[\*]                            {\p s -> TIMES p s}
\)                              {\p s -> RPAREN p s}
\(                              {\p s -> LPAREN p s}
implies                         {\p s -> IMPLIES p s}
[\~]                            {\p s -> NEGATE p s}
=                               {\p s -> EQUAL p s}
==                              {\p s -> EQUALS p s}
not                             {\p s -> NOT p s}
xor                             {\p s -> XOR p s}
eof                             {\p s -> EOF p s}
\<                              {\p s -> LESSTHAN p s}
\>                              {\p s -> GREATERTHAN p s}
[\|]{2}                         {\p s -> OR p s}
&&                              {\p s -> AND p s}
xor                             {\p s -> XOR p s}
if                              {\p s -> IF p s}
else                            {\p s -> ELSE p s}
then                            {\p s -> THEN p s}
fi                              {\p s -> FI p s}
end                             {\p s -> END p s}
TRUE|FALSE                      {\p s -> CONST p s}
$alpha [$alpha $digit \_ \’]*   {\p s -> ID p s}
$digit+                         {\p s -> NUM p s}


{
data Token = 
    EOF AlexPosn String|
    END AlexPosn String|
    NOT AlexPosn String|
    LET AlexPosn String|
    IN AlexPosn String|
    EQUALS AlexPosn String|
    EQUAL AlexPosn String|
    GREATERTHAN AlexPosn String|
    LESSTHAN AlexPosn String|
    OR AlexPosn String|
    AND AlexPosn String|
    XOR AlexPosn String|
    IF AlexPosn String|
    ELSE AlexPosn String|
    THEN AlexPosn String|
    FI AlexPosn String|
    CONST AlexPosn String|
    IMPLIES AlexPosn String|
    RPAREN AlexPosn String|
    LPAREN AlexPosn String| 
    ID AlexPosn String| 
    PLUS AlexPosn String| 
    MINUS AlexPosn String| 
    TIMES AlexPosn String| 
    NEGATE AlexPosn String| 
    NUM AlexPosn String deriving (Eq,Show)



first (EOF p s) = s
first (NOT p s) = s
first (LET p s) = s
first (IN p s) = s
first (EQUAL p s) = s
first (EQUALS p s) = s
first (GREATERTHAN p s) = s
first (LESSTHAN p s) = s
first (OR p s) = s
first (AND p s) = s
first (XOR p s) = s
first (IF p s) = s
first (ELSE p s) = s 
first (THEN p s) = s 
first (FI p s) = s
first (CONST p s) = s
first (IMPLIES p s) = s
first (LPAREN p s) = s
first (RPAREN p s) = s
first (ID p s) = s
first (PLUS p s) = s
first (MINUS p s) = s
first (TIMES p s) = s
first (NEGATE p s) = s
first (NUM p s) = s
first (END p s) = s


tokenPosn (EOF (AlexPn _ line col) s) = "<line : "++show line ++ "> <col : " ++ show col ++ "> : "   
tokenPosn (END (AlexPn _ line col) s) = "<line : "++show line ++ "> <col : " ++ show col ++ "> : "   
tokenPosn (NOT (AlexPn _ line col) s) = "<line : "++show line ++ "> <col : " ++ show col ++ "> : "   
tokenPosn (LET (AlexPn _ line col) s) = "<line : "++show line ++ "> <col : " ++ show col ++ "> : "   
tokenPosn (IN (AlexPn _ line col) s) = "<line : "++show line ++ "> <col : " ++ show col ++ "> : "   
tokenPosn (EQUAL (AlexPn _ line col) s) = "<line : "++show line ++ "> <col : " ++ show col ++ "> : "   
tokenPosn (EQUALS (AlexPn _ line col) s) = "<line : "++show line ++ "> <col : " ++ show col ++ "> : "   
tokenPosn (GREATERTHAN (AlexPn _ line col) s) = "<line : "++show line ++ "> <col : " ++ show col ++ "> : "   
tokenPosn (LESSTHAN (AlexPn _ line col) s) = "<line : "++show line ++ "> <col : " ++ show col ++ "> : "   
tokenPosn (OR (AlexPn _ line col) s) = "<line : "++show line ++ "> <col : " ++ show col ++ "> : "   
tokenPosn (AND (AlexPn _ line col) s) = "<line : "++show line ++ "> <col : " ++ show col ++ "> : "   
tokenPosn (XOR (AlexPn _ line col) s) = "<line : "++show line ++ "> <col : " ++ show col ++ "> : "   
tokenPosn (IF (AlexPn _ line col) s) = "<line : "++show line ++ "> <col : " ++ show col ++ "> : "   
tokenPosn (ELSE (AlexPn _ line col) s) = "<line : "++show line ++ "> <col : " ++ show col ++ "> : "   
tokenPosn (THEN (AlexPn _ line col) s) = "<line : "++show line ++ "> <col : " ++ show col ++ "> : "    
tokenPosn (FI (AlexPn _ line col) s) = "<line : "++show line ++ "> <col : " ++ show col ++ "> : "   
tokenPosn (CONST (AlexPn _ line col) s) = "<line : "++show line ++ "> <col : " ++ show col ++ "> : "   
tokenPosn (IMPLIES (AlexPn _ line col) s) = "<line : "++show line ++ "> <col : " ++ show col ++ "> : "   
tokenPosn (LPAREN (AlexPn _ line col) s) = "<line : "++show line ++ "> <col : " ++ show col ++ "> : "   
tokenPosn (RPAREN (AlexPn _ line col) s) = "<line : "++show line ++ "> <col : " ++ show col ++ "> : "   
tokenPosn (ID (AlexPn _ line col) s) = "<line : "++show line ++ "> <col : " ++ show col ++ "> : "   
tokenPosn (PLUS (AlexPn _ line col) s) = "<line : "++show line ++ "> <col : " ++ show col ++ "> : "   
tokenPosn (MINUS (AlexPn _ line col) s) = "<line : "++show line ++ "> <col : " ++ show col ++ "> : "   
tokenPosn (TIMES (AlexPn _ line col) s) = "<line : "++show line ++ "> <col : " ++ show col ++ "> : "   
tokenPosn (NEGATE (AlexPn _ line col) s) = "<line : "++show line ++ "> <col : " ++ show col ++ "> : "   
tokenPosn (NUM (AlexPn _ line col) s) = "<line : "++show line ++ "> <col : " ++ show col ++ "> : "   


isNumToken (NUM _ _) = True
isNumToken _ = False

isEndToken (END _ _) = True
isEndToken _ = False


isNegateToken (NEGATE _ _) = True
isNegateToken _ = False

isTimesToken (TIMES _ _) = True
isTimesToken _ = False

isMinusToken (MINUS _ _) = True
isMinusToken _ = False

isPlusToken (PLUS _ _) = True
isPlusToken _ = False

isIdToken (ID _ _) = True
isIdToken _ = False

isLparenToken (LPAREN _ _) = True
isLparenToken _ = False

isRparenToken (RPAREN _ _) = True
isRparenToken _ = False

isInToken (IN _ _) = True
isInToken _ = False

isImpliesToken (IMPLIES _ _) = True
isImpliesToken _ = False

isConstToken (CONST _ _) = True
isConstToken _ = False

isIfToken (IF _ _) = True
isIfToken _ = False

isfiToken (FI _ _) = True
isfiToken _ = False

iselseToken (ELSE _ _) = True
iselseToken _ = False

isthenToken (THEN _ _) = True
isthenToken _ = False

isAndToken (AND _ _) = True
isAndToken _ = False

isXorToken (XOR _ _) = True
isXorToken _ = False

isOrToken (OR _ _) = True
isOrToken _ = False

isLtToken (LESSTHAN _ _) = True
isLtToken _ = False

isGtToken (GREATERTHAN _ _) = True
isGtToken _ = False

isEqualsToken (EQUALS _ _) = True
isEqualsToken _ = False

isEqualToken (EQUAL _ _) = True
isEqualToken _ = False

isLetToken (LET _ _) = True
isLetToken _ = False

isEofToken (EOF _ _) = True
isEofToken _ = False

isNotToken (NOT _ _) = True
isNotToken _ = False


consumeFirst (x: xs) = xs

getErrorString s =
    let
        getErrorString1 s ans =
            case s of 
            [] -> ans
            (x: xs) -> if x == ' ' || x == '\n'
                       then ans
                       else getErrorString1 (xs) (ans ++ [x])
    in
        getErrorString1 s []

myAlexScanTokens str0 = go (alexStartPos,'\n',[],str0)
  where go inp__@(pos,_,_,str) =
          case alexScan inp__ 0 of
                AlexEOF -> []
                AlexError ((AlexPn offset line column),_,_,myStr) -> error $ "Unknown Token:<line no:" ++ (show line) ++ "> <column no:" ++ (show column) ++ "> <token" ++ (show (getErrorString myStr)) ++ ">"
                AlexSkip  inp__' _ln     -> go inp__'
                AlexToken inp__' len act -> act pos (take len str) : go inp__'

}   