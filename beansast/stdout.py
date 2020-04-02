from .psrgmrparser import ASTNode
from color import *

def normalize(t, total):
    return int(round(t/total*100, 0))

def print_times_r(t1,t2,t3):
    total = t3-t1
    t3 -= t2
    t2 -= t1
    print("Total time: %ss" % total)
    print(" - lex time: %ss (%s%%)" % (t2, normalize(t2, total)))
    print(" - parse time: %ss (%s%%)" % (t3, normalize(t3, total)))
def print_times(t1, t2, t3, t4):
    total = t4 - t1
    t4 -= t3
    t3 -= t2
    t2 -= t1
    print("Total time: %ss" % total)
    print(" - lex time: %ss (%s%%)" % (t2, normalize(t2,total)))
    print(" - initialise parse time: %ss (%s%%)" % (t3, normalize(t3,total)))
    print(" - parse time: %ss (%s%%)" % (t4, normalize(t4, total)))

def print_trie_node(node, offset=0, name="Node"): 
    print("  "*offset + "%s: " % name + (repr(node.rules) if node.rules != [] else ""))
    for key, node in node.children.items():
        print_trie_node(node, offset+1,name=str(key))
    
def print_trie(trie):
    print("Trie:")
    print_trie_node(trie.root_node)
    
def print_tree(tree, show_private=False):
    print("Abstract Syntax Tree:")
    for node in tree:
        print_node(node, show_private=show_private)
    print("--\n")

def print_stack(stack, flux, pos):
    for node in stack:
        print(node.name)
    print("[" + flux[pos+1].name + "]")
    
def print_node(node, depth=0, key=None, last_char="├─", last_bar=None, show_private=False):
    if not last_bar:
        last_bar = ["  "]
    if key: key = key + ": "
    else: key = ""
    print("".join(last_bar[:-1]) + (last_char * bool(depth)) + key + node.name)
    last_bar.append("│ ")
    last_char = "├─"
    attributes = node.attributes.copy()
    todel = tuple(key for key, value in attributes.items() if type(value) != list and (value._value == "[None]" or (value._private and not show_private)))
    for key in todel:
        del attributes[key]
    max_len = 0
    for attribute in attributes.values():
        if type(attribute) == list:
            max_len += len(attribute)
        else:
            max_len += 1
    i = 0
    for (key, value) in attributes.items():
        if i == max_len - 1:
            last_char = "└─"
            last_bar[depth+1] = "  "
        if type(value) == list:
            for node in value:
                if i == max_len - 1:
                    last_char = "└─"
                    last_bar[depth+1] = "  "
                i += 1
                print_node(node, depth+1, key, last_char, last_bar.copy(), show_private=show_private)
        elif type(value._value) == ASTNode:
            i += 1
            print_node(value, depth+1, key, last_char, last_bar.copy(), show_private=show_private)
        else:
            i += 1
            print("".join(last_bar[:-1]) + last_char + key + ": " + repr(value))

