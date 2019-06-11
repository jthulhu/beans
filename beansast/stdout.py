from .psrgmrparser import ASTNode
from .color import *

def print_tree(tree):
    print("Abstract Syntax Tree:")
    for node in tree:
        print_node(node)
    print("--\n")

def print_node(node, depth=0, key=None, last_char="├─", last_bar=None):
    if not last_bar:
        last_bar = ["  "]
    if key: key = key + ": "
    else: key = ""
    print("".join(last_bar[:-1]) + (last_char * bool(depth)) + key + node.name)
    last_bar.append("│ ")
    last_char = "├─"
    attributes = node.attributes.copy()
    todel = tuple(key for key, value in attributes.items() if value == "[None]")
    for key in todel:
        del attributes[key]
    max_len = len(attributes)
    for i, (key, value) in enumerate(attributes.items()):
        if i == max_len - 1:
            last_char = "└─"
            last_bar[depth+1] = "  "
        if type(value) == ASTNode:
            print_node(value, depth+1, key, last_char, last_bar.copy())
        else:
            print("".join(last_bar[:-1]) + last_char + key + ": " + repr(value))

