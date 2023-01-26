import ply.lex as lex
import ply.yacc as yacc


'''
Grammar:

program         : PROGRAM identifier declarations compound-statement

declarations    : VAR declaration-list
                | empty
                
declaration-list: identifier-list COLON type
                | identifier-list COMMA identifier
                
type            : INTEGER
                | REAL
                
compound-statement : BEGIN statement-list END

statement-list  : statement
                | statement-list SEMICOLON statement

statement       : identifier ASSIGN expression
                | IF expression THEN statement ELSE statement 
                | IF expression THEN statement 
                | WHILE expression DO statement 
                | compound-statement
                | PRINT LPAREN expression RPAREN
                | SWITCH expression OF cases default-cases DONE
                
default-cases   : DEFAULT statement SEMICOLON
                | empty

cases           : constant-list COLON statement SEMICOLON cases
                | empty

constant-list   : constant 
                | constant-list COMMA constant      

constant        : real 
                | integer

expressions     : integer
                | real
                | identifier
                | expression PLUS expression
                | expression MINUS expression
                | expression MUL expression
                | expression DIV expression
                | MINUS expression
                | expression MOD expression
                | expression LT expression
                | expression EQ expression
                | expression GT expression
                | expression NEQ expression
                | expression LTEQ expression
                | expression GTEQ expression
                | expression AND expression
                | expression OR expression
                | NOT expression
                | LPAREN expression RPAREN



'''