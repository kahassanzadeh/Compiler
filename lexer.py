import ply.lex as lex
import ply.yacc as yacc

'''
Grammar:

program         : PROGRAM identifier declarations compound-statement

declarations    : VAR declaration-list
                | empty
                
declaration-list: identifier-list COLON type
                | declaration-list SEMICOLON identifier-list COLON type
                
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


# Parsing rules

precedence = (
    ('right','AND'),
    ('right','OR'),
    ('right','NOT'),
    ('right','PLUS','MINUS'),
    ('right','TIMES','DIVIDE'), # TODO: mod shuold have the same precedence as these, somehow.
    ('nonassoc', 'MOD')
    ('right','UMINUS'),
    )

# dictionary of names
names = { }

int_identifiers = []
float_identifiers = []
tmp_int_ids = []
tmp_float_ids = []

def p_program(t):
    '''program: PROGRAM identifier declarations compound-statement'''
    t[0].code = "#include <stdio.h>\n" + t[2].code + t[3].code + t[4].code
    print(t[0].code)

def p_decls_decllist(t):
    '''declarations : VAR declaration-list'''
    t[0].code = ""
    if(len(int_identifiers) != 0):
        t[0].code += "int "
        for id in int_identifiers:
            t[0].code += id + ", "
        t[0].code = t[0].code[:-2] + ";\n"

    if(len(int_identifiers) != 0):
        t[0].code += "float "
        for id in int_identifiers:
            t[0].code += id + ", "
        t[0].code = t[0].code[:-2] + ";\n"
        
def p_decls_decllist_empty(t):
    '''declarations : empty'''
    t[0].code = ""
    

def p_decllist_idlist_type(t):
    '''declaration-list : identifier-list COLON type'''
    if t[3].value == "int":
        int_identifiers.append(t[1].ids)
    elif t[3].value == "real":
        float_identifiers.append(t[1].ids)



def p_decllist_idlist_more(t):
    '''declaration-list : declaration-list SEMICOLON identifier-list COLON type'''
    if t[5].value == "int":
        int_identifiers.append(t[3].ids)
    elif t[5].value == "real":
        float_identifiers.append(t[3].ids)

def p_idlist_id(t):
    '''identifier-list : identifier'''
    t[0].ids = [t[1].value] 

def p_idlist_more(t):
    '''identifier-list : identifier-list COMMA identifier'''
    t[0].ids = []
    t[0].ids.append(t[1].ids)
    t[0].ids.append(t[3].value)

def p_type(t):
    '''type : INTEGER
            | REAL'''
    t[0].type = t[1].value

def p_compstmt_stmtlist(t):
    '''compound-statement : BEGIN statement-list END'''

def p_stmtlist_stmt(t):
    '''statement-list : statement
                      | statement-list SEMICOLON statement'''

def p_statement(t):
    '''statement : identifier ASSIGN expression
                 | IF expression THEN statement ELSE statement 
                 | IF expression THEN statement 
                 | WHILE expression DO statement 
                 | compound-statement
                 | PRINT LPAREN expression RPAREN
                 | SWITCH expression OF cases default-cases DONE'''

def p_defcases(t):
    '''default-cases : DEFAULT statement SEMICOLON
                     | empty'''

def p_cases(t):
    '''cases : constant-list COLON statement SEMICOLON cases
             | empty'''

def p_constant_list(t):
    '''constant-list : constant 
                     | constant-list COMMA constant '''

def p_constant(t):
    '''constant : real 
                | integer'''

def p_expressions_term(t):
    '''expressions : integer
                   | real
                   | identifier'''
def p_expressions_op(t):
       '''expressions : expression PLUS expression
                   | expression MINUS expression
                   | expression MUL expression
                   | expression DIV expression
                   | MINUS expression'''
        
def p_expressions_mod(t):
       '''expressions : expression MOD expression'''
def p_expressions_relop(t):
       '''expressions : expression LT expression
                   | expression EQ expression
                   | expression GT expression
                   | expression NEQ expression
                   | expression LTEQ expression
                   | expression GTEQ expression'''
def p_expressions_logic(t):
       '''expressions : expression AND expression
                   | expression OR expression
                   | NOT expression'''
def p_expressions_paren(t):
       '''expressions : LPAREN expression RPAREN'''

def p_error(t):
    print("Syntax error at '%s'" % t.value)


import ply.yacc as yacc
parser = yacc.yacc()

while True:
    try:
        s = input('calc > ')   # Use raw_input on Python 2
    except EOFError:
        break
    parser.parse(s)