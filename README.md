# YawN
#### Yes also with No
This is an interpreter for boolean logic or discrete logic. You enter expressions and it will print a truth table of all possible values the expressions can have given the values of the variables in the expression. 
## Usage
Conjunction:
```
YAWN> (a ^ b)
```
Disjunction:
```
YAWN> (a v b)
YAWN> (a V b)
```
Negation:
```
YAWN> (~ a)
```
Material Conditional:
```
YAWN> (a -> b)
```
Material Biconditional:
```
YAWN> (a <-> b)
```
Exclusive OR:
```
YAWN> (a xv b)
YAWN> (a XV b)
```
YawN will also inform you if a given expression is a tautology or contradiction, or if two expressions are equal.   

One note: Be sure to use parenthesis in abundence. The interpreter reads in linear order and does not understand order of operations.
## Demo


https://github.com/qeftser/yawn/assets/144874443/7a509610-ea0b-45ed-9f26-d5dec3f500fd

