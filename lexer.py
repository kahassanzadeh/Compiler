import ply.lex as lex
import ply.yacc as yacc
import Parse

program = '''
    program abcad var abc : int ; a , b : real begin abc := 2 + 3 * 3 end
   '''
""" program = '''
program abcad var abc : int ; a : int begin while a < b do a := 2 + 3 * 3 end
''' """

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
             'PERIOD'
         ] + list(reserved.values())

t_ignore = ' \t'
t_ASSIGN = r'\:='
t_PLUS = r'\+'
t_MINUS = r'-'
t_MUL = r'\*'
t_DIV = r'/'
t_GT = r'>'
t_LT = r'<'
t_EQ = r'='
t_NEQ = r'<>'
t_GTEQ = r'>='
t_LTEQ = r'<='
t_INTEGER_CONSTANT = r'\d+([uU]|[lL]|[uU][lL]|[lL][uU])?'
t_REAL_CONSTANT = r'((\d+)(\.\d+)(e(\+|-)?(\d+))? | (\d+)e(\+|-)?(\d+))([lL]|[fF])?'
t_SEMICOLON = r';'
t_COLON = r':'
t_COMMA = r','
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_PERIOD = r'\.'


# function for getting the integer value
# def t_INTEGER_CONSTANT(t):
#     r'(((((0x)|(0X))[0-9a-fA-F]+)|(\d+))([uU][lL]|[lL][uU]|[uU]|[lL])?)'
#     t.value = int(t.value)
#     return t


# function for getting either the reserved word or identifier
def t_ID(t):
    r'[A-Za-z_][A-Za-z0-9_]*'
    t.type = reserved.get(t.value, 'ID')
    return t


# def t_REAL_CONSTANT(t):
#     r'((\d+)(\.\d+)(e(\+|-)?(\d+))? | (\d+)e(\+|-)?(\d+))([lL]|[fF])?'
#     t.value = float(t.value)
#     return t


def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)


def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


lexer = lex.lex()

lexer.input(program)

while True:
    tok = lexer.token()
    if not tok:
        break
    #print(tok)

precedence = (
    ("nonassoc", 'IF'),
    ("nonassoc", 'ELSE'),
    ('right', 'AND'),
    ('right', 'OR'),
    ('right', 'NOT'),
    ('right', 'PLUS', 'MINUS'),
    ('right', 'MUL', 'DIV', 'MOD'),
    ('right', 'UMINUS')
)

# dictionary of names
names = {}

start = 'program'

int_identifiers = []
float_identifiers = []
tmp_int_ids = []
tmp_float_ids = []

def new_tmp(typeof):
    if(typeof == "int"):
        current_count = len(tmp_int_ids)
        tmp_int_ids.append("temp_int_" + str(current_count + 1))
        return tmp_int_ids[-1]
    elif(typeof == "float"):
        current_count = len(tmp_float_ids)
        tmp_float_ids.append("temp_float_" + str(current_count + 1))
        return tmp_float_ids[-1]


def p_program(t):
    '''program : PROGRAM ID declarations compound-statement'''
    t[0] = Parse.ParseObj()
    t[0].code = "#include <stdio.h>\n" + t[3].code + "int main()\n{\n" + t[4].code + "\n" + "}"
    print(t[0].code)


def p_decls_decllist(t):
    '''declarations : VAR declaration-list'''
    t[0] = Parse.ParseObj()
    t[0].code = ""
    if len(int_identifiers) != 0:
        t[0].code += "int "
        for id in int_identifiers:
            t[0].code += id[0] + ", "
        t[0].code = t[0].code[:-2] + ";\n"

    if len(int_identifiers) != 0:
        t[0].code += "float "
        for id in int_identifiers:
            t[0].code += id[0] + ", "
        t[0].code = t[0].code[:-2] + ";\n"


def p_decls_decllist_empty(t):
    '''declarations : '''
    t[0] = Parse.ParseObj()
    t[0].code = ""


def p_decllist_idlist_type(t):
    '''declaration-list : identifier-list COLON type'''
    if t[3].type == "int":
        int_identifiers.append(t[1].ids)
    elif t[3].type == "real":
        float_identifiers.append(t[1].ids)


def p_decllist_idlist_more(t):
    '''declaration-list : declaration-list SEMICOLON identifier-list COLON type'''
    if t[5].type == "int":
        int_identifiers.append(t[3].ids)
    elif t[5].type == "real":
        float_identifiers.append(t[3].ids)


def p_idlist_id(t):
    '''identifier-list : ID'''
    t[0] = Parse.ParseObj()
    t[0].ids = [t[1]]


def p_idlist_more(t):
    '''identifier-list : identifier-list COMMA ID'''
    t[0] = Parse.ParseObj()
    t[0].ids = []
    t[0].ids.append(t[1].ids)
    t[0].ids.append(t[3])


def p_type(t):
    '''type : INTEGER
            | REAL'''
    t[0] = Parse.ParseObj()
    t[0].type = t[1]


def p_compstmt_stmtlist(t):
    '''compound-statement : BEGIN statement-list END'''
    t[0] = Parse.ParseObj()
    t[0].code = t[2].code


def p_stmtlist_stmt(t):
    '''statement-list : statement'''
    t[0] = Parse.ParseObj()
    t[0].code = t[1].code


def p_stmtlist_stmt_more(t):
    '''statement-list : statement-list SEMICOLON statement'''
    t[0] = Parse.ParseObj()
    t[0].code = t[1].code + "\n" + t[3].code


def p_statement_compstmt(t):
    '''statement : compound-statement'''
    t[0] = Parse.ParseObj()
    t[0].code = t[1].code


def p_statement_assign(t):
    '''statement : ID ASSIGN expression'''
    t[0] = Parse.ParseObj()
    t[0].code = t[3].code + t[1] + "=" + t[3].address + ";"


def p_statement_ifthenelse(t):
    '''statement : IF expression THEN statement ELSE statement '''


def p_statement_ifthen(t):
    '''statement : IF expression THEN statement %prec IF'''
    t[0] = Parse.ParseObj()
    t[0].code = 'label_' + str(t.lexer.lineno) + ' if (' + t[2].code + ') ' + 'goto label_' + str(t.lexer.lineno + 2) + '\n' + 'goto label_' + str(
        t.lexer.lineno + 3) + '\n' + 'label_'+ str(t.lexer.lineno + 2) + ' ' + t[4].code + '\n'
    t.lexer.lineno += 3

def p_statement_whiledo(t):
    '''statement : WHILE expression DO statement'''
    t[0] = Parse.ParseObj()
    t[0].code = 'label_' + str(t.lexer.lineno) + ' if (' + t[2].code + ') ' + 'goto label_' + str(t.lexer.lineno + 2) + '\n' + 'goto label_' + str(
        t.lexer.lineno + 3) + '\n' + 'label_'+ str(t.lexer.lineno + 2) + ' ' + t[4].code + '\n' + 'goto label_' + str(t.lexer.lineno)


def p_statement_print(t):
    '''statement : PRINT LPAREN expression RPAREN'''


# switch case stuff
""" def p_statement_switch(t):
    '''statement : SWITCH expression OF cases default-cases DONE'''
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
                | integer''' """


def p_expressions_int(t):
    '''expression : INTEGER_CONSTANT'''
    t[0] = Parse.ParseObj()
    t[0].code = ""
    t[0].address = t[1]
    t[0].type = "int"

def p_expressions_float(t):
    '''expression : REAL_CONSTANT'''
    t[0] = Parse.ParseObj()
    t[0].code = ""
    t[0].address = t[1]
    t[0].type = "float"

def p_expressions_id(t):
    '''expression : ID'''
    t[0] = Parse.ParseObj()
    t[0].code = ""
    t[0].address = t[1]
    if [t[0].code] in int_identifiers:
        t[0].type = "int"
    elif [t[0].code] in float_identifiers:
        t[0].type = "int"
    else:
        raise SyntaxError

def p_expressions_op(t):
    '''expression : expression PLUS expression
                   | expression MINUS expression
                   | expression MUL expression
                   | expression DIV expression
                   '''
    t[0] = Parse.ParseObj()
    type1 = t[1].type
    type2 = t[3].type
    t[0].type = "int"
    if (type1 == 'float') or (type2 == "float"):
        t[0].type = "float"
    t[0].address = new_tmp(t[0].type)
    t[0].code = t[1].code + t[3].code + t[0].address + " = " + t[1].address + t[2] + t[3].address + ";\n"


def p_expressions_umin(t):
    '''expression : MINUS expression %prec UMINUS'''
    t[0] = Parse.ParseObj()
    t[0].type = t[2].type
    t[0].address = new_tmp(t[2].type)
    t[0].code = t[2].code + t[0].address + " = " + t[1] + t[2].address + ";\n"


def p_expressions_mod(t):
    '''expression : expression MOD expression'''
    # t[0] = t[1] % t[3]
    t[0] = Parse.ParseObj()
    type1 = t[1].type
    type2 = t[2].type
    t[0].type = "int"
    if (type1 == 'float') or (type2 == "float"):
        raise SyntaxError
    t[0].address = new_tmp(t[0].type)
    t[0].code = t[1].code + t[3].code + t[0].address + " = " + t[1].address + "%" + t[3].address + ";\n"


def p_expressions_relop(t):
    '''expression : expression LT expression
                   | expression EQ expression
                   | expression GT expression
                   | expression NEQ expression
                   | expression LTEQ expression
                   | expression GTEQ expression'''
    t[0] = Parse.ParseObj()
    t[0].code = t[1].code + t[2] + t[3].code


def p_expressions_logic(t):
    '''expression : expression AND expression
                   | expression OR expression'''
    t[0] = Parse.ParseObj()
    type1 = t[1].type
    type2 = t[3].type
    t[0].type = "int"
    if (type1 == 'float') or (type2 == "float"):
        raise SyntaxError
    t[0].address = new_tmp(t[0].type)
    t[0].code = t[1].code + t[3].code + t[0].address + " = " + t[1].address + t[2] + t[3].address + ";\n"

def p_expressions_not(t):
    '''expression : NOT expression'''
    t[0] = Parse.ParseObj()
    t[0].type = t[2].type
    t[0].address = new_tmp(t[2].type)
    t[0].code = t[2].code + t[0].address + " = " + t[1] + t[2].address + ";\n"
    
def p_expressions_paren(t):
    '''expression : LPAREN expression RPAREN'''
    t[0] = Parse.ParseObj()
    t[0].address = t[2].address
    t[0].type = t[2].type
    t[0].code = "(" + t[2].code + ")"


def p_error(t):
    print("Syntax error at '%s'" % t.value)

parser = yacc.yacc()
parser.parse(program)