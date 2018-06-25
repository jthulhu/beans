#!/usr/bin/python3
# -*- coding: utf-8 -*-

import beansast

#############
### LEXER ###
#############

lexer = beansast.Lexer('a')
a = lexer.lex()
assert a.code == beansast.primitives["VARIABLE"]["code"]
a = lexer.lex()
assert a.code == beansast.primitives["EOF"]["code"]

lexer = beansast.Lexer('b        ="zadpojazd"')
a = lexer.lex()
assert a.code == beansast.primitives["VARIABLE"]["code"]
assert a.value == "b"
a = lexer.lex()
assert a.code == beansast.primitives["EQUALS"]["code"]
a = lexer.lex()
assert a.code == beansast.primitives["STRING"]["code"]
assert a.value == "zadpojazd"
a = lexer.lex()
assert a.code == beansast.primitives["EOF"]["code"]

lexer = beansast.Lexer(';')
a = lexer.lex()
assert a.code == beansast.primitives["SEMICOLON"]["code"]
a = lexer.lex()
assert a.code == beansast.primitives["EOF"]["code"]

lexer = beansast.Lexer("function b: ();")
a = lexer.lex()
assert a.code == beansast.primitives["FUNCTION"]["code"]
a = lexer.lex()
assert a.code == beansast.primitives["VARIABLE"]["code"]
assert a.value == "b"
a = lexer.lex()
assert a.code == beansast.primitives["COLON"]["code"]
a = lexer.lex()
assert a.code == beansast.primitives["LPAR"]["code"]
a = lexer.lex()
assert a.code == beansast.primitives["RPAR"]["code"]
a = lexer.lex()
assert a.code == beansast.primitives["SEMICOLON"]["code"]
a = lexer.lex()
assert a.code == beansast.primitives["EOF"]["code"]

lexer = beansast.Lexer("while")
a = lexer.lex()
assert a.code == beansast.primitives["WHILE"]["code"]
a = lexer.lex()
assert a.code == beansast.primitives["EOF"]["code"]

##############
### PARSER ###
##############


code = "print(a, c(b))"
lexer = beansast.Lexer(code)
parser = beansast.Parser(lexer)
result = parser.parse()
assert type(result[0]) is beansast.FunctionCallNode
assert result[0].name == "print"
assert type(result[0].arguments[0]) == beansast.VariableNode
assert result[0].arguments[0].name == "a"
assert type(result[0].arguments[1]) == beansast.FunctionCallNode
assert result[0].arguments[1].name == "c"
assert type(result[0].arguments[1].arguments[0]) == beansast.VariableNode
assert result[0].arguments[1].arguments[0].name == "b"


code = """function a: a, b {print(a, b)}"""
result = beansast.Parser(beansast.Lexer(code)).parse()
assert type(result[0]) is beansast.FunctionDefinitionNode
assert result[0].name is 'a'
assert type(result[0].args[0]) is beansast.VariableNode
assert result[0].args[0].name is 'a'
assert type(result[0].args[1]) is beansast.VariableNode
assert result[0].args[1].name is 'b'
assert type(result[0].commands[0]) is beansast.FunctionCallNode
assert type(result[0].commands[0].arguments[0]) is beansast.VariableNode
assert result[0].commands[0].arguments[0].name == "a"
assert type(result[0].commands[0].arguments[1]) is beansast.VariableNode
assert  result[0].commands[0].arguments[1].name == "b"

code = """class a: {
method b: {} method c: {}
}
"""

result = beansast.Parser(beansast.Lexer(code)).parse()
assert type(result[0]) is beansast.ClassDefinitionNode
assert result[0].name is 'a'
assert type(result[0]) is beansast.ClassDefinitionNode


code = """a = a.b.c.d().e().f"""

result = beansast.Parser(beansast.Lexer(code)).parse()
assert len(result) == 1
assert type(result[0]) is beansast.AssignmentNode
assert result[0].name is 'a'
ctnt = result[0].value
assert type(ctnt) is beansast.VariableNode
assert ctnt.name is 'a'
ctnt = ctnt.child
assert type(ctnt) is beansast.VariableNode
assert ctnt.name is 'b'
ctnt = ctnt.child
assert type(ctnt) is beansast.VariableNode
assert ctnt.name is 'c'
ctnt = ctnt.child
assert type(ctnt) is beansast.FunctionCallNode
assert ctnt.name is 'd'
assert ctnt.arguments == []
ctnt = ctnt.child
assert type(ctnt) is beansast.FunctionCallNode
assert ctnt.name is 'e'
assert ctnt.arguments == []
ctnt = ctnt.child
assert type(ctnt) is beansast.VariableNode
assert ctnt.name is 'f'
ctnt = ctnt.child
assert ctnt is None

code = '{"a": ["a", "b"], "c": "d", e: g}'
result = beansast.Parser(beansast.Lexer(code)).parse()
assert len(result) == 1
assert type(result[0]) is beansast.DictNode
ctnt = result[0]
#assert ctnt.value[

print("Everything good!")
