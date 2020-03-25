#!/usr/bin/python3

from beansast import psrgmrparser, lexer
from carrot import *

proxy = OrderedDict(
    String(),                # key
    Struct(
        String(),            # value
        Bool(),              # list_append
        Int(),               # flags
        String()             # type
    )
)

rule_token = Struct(
    String(),                # name
    String(),                # attribute
    Bool(),                  # is_append
    String()                 # key
)

rule = Struct(
    List(rule_token),
    proxy
)

trie_node = Struct(
    List(rule),              # rules
    Int(),                   # id
    Dict(                    # children
        rule_token,          # token
        Int()                # child id
    )
)
trie = List(
    trie_node
)

with open("beansast/gmrs/plexer-m.gmr") as f:
    grammar = lexer.Lexer(f.read(), file="beansast/gmrs/plexer-m.gmr", helperr=True)

with open("beansast/gmrs/parser.gmr") as f:    
    grammar(f.read(), fn="beansast/gmrs/parser.gmr")

a = psrgmrparser.ParserReader(grammar,helperr=True)
nodizers = a.read()    

with open("parser.bo", "wb") as f:
    f.write(nodizers.compile())
