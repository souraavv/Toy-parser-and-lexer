# To Run code using make file

- Use command : make all : to build the lexer.hs file and compile parser.hs along with lexer module 

- Use commnad : make run filename = * : where * is the file name example test.txt

- Use command : make clean : to remove all executable and object files


# Language Specification

if exp then exp else exp fi 

let var = exp in exp end 

# output format:

- For lexer output : [Token, AlexPosn, String] -- AlexPosn is required in parser.hs during syntax error

- For parser output: Abstract Syntax Tree.

# operator and Symbol in language

End of file                     eof

Boolean And                     && 

Boolean Or is                   || 

Boolean xor is                  xor

Boolena not is                  not

less than is                    <

greater than is                 > 

equal/assign is                 =

eqauls is                       ==

LPAREN is                       (

RPAREN is                       )

TRUE is                         TRUE

FALSE is                        FALSE

Negate is                       ~

Add is                          +

Minus is                        -

Times is                        *

comment is                      --

# Testing on few test cases:

# failure
- if (x || y && z + (2 * k - p)) then (x > z) || (p > z) else if True then x else y fi fi 
- let x = 4 in y eof 
- let x = 4 eof 
- let x = 4 y = (3 || z) in if x > y then x else fi fi end eof
- (3 / 4) eof
- if if then x else y eof
- (3 + 4) > (4 * 3 - (3 + 4 * (x * k))) > (3 * 3 + (3 && )) eof
- if x > y then if y > x then y else x fi else if y + x > 0 then y * x else x - y fi eof 
- (3 + eof

# success
- (4 + 3 - 3 + (2 * 4 - (3 * 4))) eof
- let x = 4 y = 4 z = (y + z) in let t = 4 in t * 4 end end eof 
- let x = 4 y = (3 || z) in if x > y then x else y fi end eof
- if (x || y && z + (2 * k - p)) then (x > z) || (p > z) else if True then x else y fi fi eof 
- (3 + 4) > (4 * 3 - (3 + 4 * (x * k))) > (3 * 3 + (3 && k)) eof
- if x > y then if y > x then y else x fi else if y + x > 0 then y * x else x - y fi fi eof
- (3 + ~(4)) eof
- (TRUE || FALSE) && (TRUE) eof
- not x eof
- (not TRUE || FALSE) && not(~1) eof
