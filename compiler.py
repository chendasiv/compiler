#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys

import ply.lex as lex
import ply.yacc as yacc

from symtab import Scope, Symbol, SymbolTable
from fundef import Fundef
from llvmcode import *
from operand import OType, Operand

## トークン名のリスト
tokens = (
    'BEGIN', 'DIV', 'DO', 'ELSE', 'END', 'FOR', 'FUNCTION', 'IF',
    'PROCEDURE', 'PROGRAM', 'READ', 'THEN', 'TO', 'VAR', 'WHILE', 'WRITE',
    'PLUS', 'MINUS', 'MULT', 'EQ', 'NEQ', 'LT', 'LE', 'GT', 'GE',
    'LPAREN', 'RPAREN', 'LBRACKET', 'RBRACKET', 'COMMA', 'SEMICOLON',
    'PERIOD', 'INTERVAL', 'ASSIGN',
    'IDENT', 'NUMBER'
)

## 予約語の定義
reserved = {
    'begin': 'BEGIN',
    'div': 'DIV',
    'do': 'DO',
    'else': 'ELSE',
    'end': 'END',
    'for': 'FOR',
    'function': 'FUNCTION',
    'if': 'IF',
    'procedure': 'PROCEDURE',
    'program': 'PROGRAM',
    'read': 'READ',
    'then': 'THEN',
    'to': 'TO',
    'var': 'VAR',
    'while': 'WHILE',
    'write': 'WRITE'
}

## 基本シンボルトークンを切り出すルール
t_PLUS  = '\+'
t_MINUS = '-'
t_MULT  = '\*'
t_EQ = '='
t_NEQ = '<>'
t_LT = '<'
t_LE = '<='
t_GT = '>'
t_GE = '>='
t_LPAREN = '\('
t_RPAREN = '\)'
t_LBRACKET = '\['
t_RBRACKET = '\]'
t_COMMA = ','
t_SEMICOLON = ';'
t_PERIOD = '\.'
t_INTERVAL = '\.\.'
t_ASSIGN = ':='

# コメントおよび空白・タブを無視するルール
t_ignore_COMMENT = '\#.*'
t_ignore = ' \t'

## アクションを伴うトークンルール
# 変数名・手続き名などの識別子を切り出すルール
def t_IDENT(t):
    '[a-zA-Z][a-zA-Z0-9]*'
    t.type = reserved.get(t.value, 'IDENT')
    return t

# 数値を切り出すルール
def t_NUMBER(t):
    '[1-9][0-9]*|0'
    try:
        t.value = int(t.value)
    except ValueError:
        print("Line %d: integer value %s is too large" % t.lineno, t.value)
        t.value = 0
    return t

# 改行を読んだときの処理
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# エラー処理
def t_error(t):
    print("不正な文字「", t.value[0], "」")
    t.lexer.skip(1)


#################################################################
# 解析に必要な変数を宣言しておく
#################################################################

symtable = SymbolTable()
varscope = Scope.GLOBAL_VAR

fundefs = []				# 生成した関数定義（Fundef）のリスト

useWrite = False			# write関数が使用されているかのフラグ
useRead  = False			# read関数が使用されているかのフラグ

def addCode(l:LLVMCode):
    ''' 現在の関数定義オブジェクトの codes に命令 l を追加 '''
    fundefs[-1].codes.append(l)

def getRegister():
    ''' 新たなレジスタ番号をもつ Operand オブジェクトを返す '''
    return Operand(OType.NUMBERED_REG, val=fundefs[-1].getNewRegNo())

#### kadai5 ####
# ラベルカウンタ
current_label = 1
def getNewLabel():
    global current_label
    label = f"{current_label}" 
    current_label += 1
    return label


#################################################################
# ここから先に構文規則を書く
#################################################################


def p_program(p):
    '''
    program : PROGRAM IDENT SEMICOLON outblock PERIOD
    '''
    with open("result.ll", "w") as fout:
        # 大域変数ごとに common global 命令を出力
        for t in symtable.rows:
            if t.scope == Scope.GLOBAL_VAR:
                print(LLVMCodeGlobal(t.name), file=fout)
        print('', file=fout)

        # 関数定義を出力
        for f in fundefs:
            f.print(fout)

        # printfやscanf関数の宣言と書式を表す文字列定義を出力
        if useWrite:
            LLVMCodeCallPrintf.printDeclare(fout)
            LLVMCodeCallPrintf.printFormat(fout)
        if useRead:
            LLVMCodeCallScanf.printDeclare(fout)
            LLVMCodeCallScanf.printFormat(fout)


def p_outblock(p):
    '''
    outblock : var_decl_part subprog_decl_part outblock_act statement
    '''
    # 規則の右辺に outblock_act が追加されていることに注意!!

    # 還元時に「ret i32 0」命令を追加
    addCode(LLVMCodeRet('i32', Operand(OType.CONSTANT, val=0)))

def p_outblock_act(p):
    '''
    outblock_act :
    '''
    # メイン処理に対する関数定義オブジェクトを生成(名前は main とする）
    fundefs.append(Fundef('main', 'i32'))


def p_var_decl_part(p):
    '''
    var_decl_part : var_decl_list SEMICOLON
                  |
    '''
    # print("var_decl_part")

def p_var_decl_list(p):
    '''
    var_decl_list : var_decl_list SEMICOLON var_decl
                  | var_decl
    '''
    # print("var_decl_list")

def p_var_decl(p):
    '''
    var_decl : VAR id_list
    '''
    # print("var_decl")

def p_subprog_decl_part(p):
    '''
    subprog_decl_part : subprog_decl_list SEMICOLON
                      |
    '''
    # print("subprog_decl_part")

def p_subprog_decl_list(p):
    '''
    subprog_decl_list : subprog_decl_list SEMICOLON subprog_decl
                      | subprog_decl
    '''
    # print("subprog_decl_list")

def p_subprog_decl(p):
    '''
    subprog_decl : proc_decl
    '''
    # print("subprog_decl")

def p_proc_decl(p):
    '''
    proc_decl : PROCEDURE proc_name LPAREN RPAREN SEMICOLON inblock
    '''
    # print("proc_decl")

def p_proc_name(p):
    '''
    proc_name : IDENT
    '''
    global varscope
    symtable.insert(p[1], Scope.PROC)
    print(symtable.rows)
    varscope = Scope.LOCAL_VAR
    # print("proc_name", p[1])

def p_inblock(p):
    '''
    inblock : var_decl_part statement
    '''
    symtable.delete()
    print(symtable.rows)
    # print("inblock")

def p_statement_list(p):
    '''
    statement_list : statement_list SEMICOLON statement
                   | statement
    '''
    # print("statement_list")
    
def p_statement(p):
    '''
    statement : assignment_statement
              | if_statement
              | while_statement
              | for_statement
              | proc_call_statement
              | null_statement
              | block_statement
              | read_statement
              | write_statement
    '''
    # print("statement")


def p_assignment_statement(p):
    '''
    assignment_statement : IDENT ASSIGN expression
    '''
    t = symtable.lookup(p[1])
    if t == None:
        print("No match for", p[1])
    else:
        print(t.__str__())

    if t.scope == Scope.GLOBAL_VAR:
        ptr = Operand(OType.GLOBAL_VAR, name=t.name)

    retval = p[3]
    addCode(LLVMCodeStore(retval, ptr))
    p[0] = retval

    # print("assignment_statement")

#### kadai5 ####
def p_if_statement(p):
    '''
    if_statement : IF condition if_act1 THEN statement else_statement
    '''
    # print("if_statement")
    # end_label = current_label - 1
    # addCode(LLVMCodeBr(l1=end_label))
    # addCode(LLVMCodeLabel(end_label))
    

def p_if_act1(p):
    '''
    if_act1 : 
    '''
    cond = p[-1]
    l1 = getNewLabel()
    l2 = getNewLabel()

    addCode(LLVMCodeBr(cond=cond, l1=l1, l2=l2))
    addCode(LLVMCodeLabel(l1))


#### kadai5 ####
def p_else_statement(p):
    '''
    else_statement : ELSE statement
                   |
    '''
    
    if len(p) == 1:
    # else 文がない場合
        end_label = current_label - 1
        addCode(LLVMCodeBr(l1=end_label))
        addCode(LLVMCodeLabel(end_label))
    else:
    # else 文がある場合
        end_label = getNewLabel()
        else_label = current_label - 2
        addCode(LLVMCodeBr(l1=end_label))
        addCode(LLVMCodeLabel(else_label))
    
    # print("else_statement")


#### kadai5 ####
def p_while_statement(p):
    '''
    while_statement : WHILE condition DO statement
    '''
    # print("while_statement")

#### kadai5 ####
def p_for_statement(p):
    '''
    for_statement : FOR IDENT ASSIGN expression for_act1 TO expression DO statement
    '''

def p_for_act1(p):
    '''
    for_act1 :
    '''
    t = symtable.lookup(p[-3])
    if t == None:
        print("No match for", p[-3])
    else:
        print(t.__str__())
    # print("for_statement", p[-3])


def p_proc_call_statement(p):
    '''
    proc_call_statement : proc_call_name LPAREN RPAREN
    '''
    # print("proc_call_statement")


def p_proc_call_name(p):
    '''
    proc_call_name : IDENT
    '''
    t = symtable.lookup(p[1])
    if t == None:
        print("No match for", p[1])
    else:
        print(t.__str__())
    # print("proc_call_name", p[1])

def p_block_statement(p):
    '''
    block_statement : BEGIN statement_list END
    '''
    # print("block_statement")

def p_read_statement(p):
    '''
    read_statement : READ LPAREN IDENT RPAREN
    '''
    global useRead
    useRead = True

    t = symtable.lookup(p[3])
    if t == None:
        print("No match for", p[3])
    else:
        print(t.__str__())

    if t.scope == Scope.GLOBAL_VAR:
        ptr = Operand(OType.GLOBAL_VAR, name=t.name)

    addCode(LLVMCodeCallScanf(getRegister(), ptr))

def p_write_statement(p):
    '''
    write_statement : WRITE LPAREN expression RPAREN
    '''
    global useWrite
    useWrite = True
    arg = p[3]
    addCode(LLVMCodeCallPrintf(getRegister(), arg))

def p_null_statement(p):
    '''
    null_statement : 
    '''
    # print("null")

#### kadai5 ####
def p_condition(p):
    '''
    condition : expression EQ expression
              | expression NEQ expression
              | expression LT expression
              | expression LE expression
              | expression GT expression
              | expression GE expression
    '''
    # print("condition")
    arg1 = p[1]
    arg2 = p[3]
    retval = getRegister()
    addCode(LLVMCodeIcmp(retval, CmpType.getCmpType(p[2]), arg1, arg2))
    p[0] = retval


def p_expression(p):
    '''
    expression : term
               | MINUS term
               | expression PLUS term
               | expression MINUS term
    '''
    if len(p) == 2:
        p[0] = p[1]
    else:
        if p[1] == "-":
            arg1 = 0
            arg2 = p[2]
            retval = getRegister()
            addCode(LLVMCodeSub(retval, arg1, arg2))
            p[0] = retval

        elif p[2] == "+":
            arg1 = p[1]
            arg2 = p[3]
            retval = getRegister()
            addCode(LLVMCodeAdd(retval, arg1, arg2))
            p[0] = retval

        elif p[2] == "-":
            arg1 = p[1]
            arg2 = p[3]
            retval = getRegister()
            addCode(LLVMCodeSub(retval, arg1, arg2))
            p[0] = retval


def p_term(p):
    '''
    term : factor
         | term MULT factor
         | term DIV factor
    '''
    if len(p) == 2:
        p[0] = p[1]
    else:
        if p[2] == "*":
            arg1 = p[1]
            arg2 = p[3]
            retval = getRegister()
            addCode(LLVMCodeMul(retval, arg1, arg2))
            p[0] = retval
        elif p[2] == "div":
            arg1 = p[1]
            arg2 = p[3]
            retval = getRegister()
            addCode(LLVMCodeDiv(retval, arg1, arg2))
            p[0] = retval


def p_factor(p):
    '''
    factor : var_name
           | number
           | LPAREN expression RPAREN
    '''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = p[2]


def p_var_name(p):
    '''
    var_name : IDENT
    '''
    t = symtable.lookup(p[1])
    if t == None:
        print("No match for", p[1])
    else:
        print(t.__str__())

    if t.scope == Scope.GLOBAL_VAR:
        ptr = Operand(OType.GLOBAL_VAR, name=t.name)

    retval = getRegister()
    addCode(LLVMCodeLoad(retval, ptr))
    p[0] = retval


def p_number(p):
    '''
    number : NUMBER
    '''
    p[0] = Operand(OType.CONSTANT, val=int(p[1]))

#### kadai4 ####
def p_id_list(p):
    '''
    id_list : IDENT
            | id_list COMMA IDENT
    '''
    if len(p) == 2:    # id_list : IDENT
        x = p[1]
    elif len(p) == 4:  # id_list : id_list COMMA IDENT
        x = p[3]

    symtable.insert(x, varscope)
    print(symtable.rows)
    # print("id_list", x,)


#################################################################
# 構文解析エラー時の処理
#################################################################

def p_error(p):
    if p:
        # p.type, p.value, p.linenoを使ってエラーの処理を書く
        print("Syntax error for Token:", p.type, ", with Value:", p.value, ", at Line Number:", p.lineno)
    else:
        print("Syntax error at EOF")

#################################################################
# メインの処理
#################################################################

if __name__ == "__main__":
    lexer = lex.lex(debug=0)  # 字句解析器
    yacc.yacc()  # 構文解析器

    # ファイルを開いて
    data = open(sys.argv[1]).read()
    # 解析を実行
    yacc.parse(data, lexer=lexer)
