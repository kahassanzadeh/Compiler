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

# Tokenizer
# gathering all the reserved words
reserved = {
    'program': 'PROGRAM',
    'var': 'VAR',
    'int': 'INTEGER',
    'real': 'REAL',
    'begin': 'BEGIN',
    'end': 'END',
    'if': 'IF',
    'then': 'THEN',
    'else': 'ELSE',
    'while': 'WHILE',
    'print': 'PRINT',
    'switch': 'SWITCH',
    'of': 'OF',
    'do': 'DO',
    'done': 'DONE',
    'default': 'DEFAULT',
    'and': 'AND',
    'or': 'OR',
    'mod': 'MOD',
    'not': 'NOT'
}

# gathering all the tokens
tokens = [
          'ASSIGN',
          'PLUS',
          'MINUS',
          'MUL',
          'DIV',
          'GT',
          'LT',
          'EQ',
          'NEQ',
          'GTEQ',
          'LTEQ',
          'INTEGER_CONSTANT',
          'REAL_CONSTANT',
          'ID',
          'SEMICOLON',
          'COLON',
          'COMMA',
          'LPAREN',
          'RPAREN',
          ] + list(reserved.values())

t_ignore = ' \t'


t_ASSIGN = r'\:='
t_PLUS = r'\+'
t_MINUS = r'\-'
t_MUL = r'\*'
t_DIV = r'\/'
t_GT = r'\>'
t_LT = r'\<'
t_EQ = r'\='
t_NEQ = r'\<>'
t_GTEQ = r'\>='
t_LTEQ = r'\<='
# t_INTEGER_CONSTANT = r'\d+'
# t_REAL_CONSTANT = r'^[+-]?([0-9]*[.])?[0-9]+$'
t_SEMICOLON = r'\;'
t_COLON = r'\:'
t_COMMA = r'\,'
t_LPAREN = r'\('
t_RPAREN = r'\)'


# function for getting the integer value
def t_INTEGER_CONSTANT(t):
    r'\d+'
    t.value = int(t.value)
    return t

# function for getting either the reserved word or identifier
def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'ID')
    return t

def t_REAL_CONSTANT(t):
    r'^[+-]?([0-9]*[.])?[0-9]+$'
    t.value = float(t.value)
    return t


def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)


def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


lexer = lex.lex()

data = '''
    if a = b
        print("salam");
  '''

lexer.input(data)

while True:
    tok = lexer.token()
    if not tok:
        break
    print(tok)
