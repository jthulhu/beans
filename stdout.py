from color import *
from beansast import parser

###############
#### TIMES ####
###############

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


####################
## EARLEY PARSING ##
####################
    
def print_ei(item, grammar):
    rule = grammar[item.rule_id]
    is_dot_placed = False
    r = []
    for i in range(len(rule)+1):
        if not is_dot_placed and i == item.dot:
            r.append('•')
            is_dot_placed = True
            continue
        if is_dot_placed:
            r.append(rule[i-1].name)
        else:
            r.append(rule[i].name)
    string = "{name} -> {tokens} ({start})".format(
        name=rule.name,
        tokens=" ".join(r),
        start=item.start
    )
    print(string)

def print_S(S, grammar):
    for i, state_set in enumerate(S):
        print('=== %s ===' % i)
        for ei in state_set:
            print_ei(ei, grammar)
        print()

def print_fei(item, grammar):
    print(
        "{name} -> {tokens} ({end})".format(
            name=grammar[item.rule_id].name,
            tokens=" ".join(token.name for token in grammar[item.rule_id]),
            end=item.end
        )
    )
        
def print_rS(rS, grammar):
    for i, state_set in enumerate(rS):
        print('=== %s ===' % i)
        for fei in state_set:
            print_fei(fei, grammar)
        print()

def print_tree(tree, show_private=False):
    print("Abstract Syntax Tree:")
    print_node(tree, show_private=show_private,first=True)
    print("--\n")
    
def print_node(node, depth=0, key=None, last_char="├─", last_bar=None, show_private=False,first=False):
    if not last_bar:
        last_bar = ["  "]
    if key: key = key + ": "
    else: key = ""
    if first:
        print('AST')
    else:
        print("".join(last_bar[:-1]) + (last_char * bool(depth)) + key)
    last_bar.append("│ ")
    last_char = "├─"
    attributes = node.attributes.copy()
    max_len = len(attributes)
    for i, (key, value) in enumerate(attributes.items()):
        if i == max_len - 1:
            last_char = "└─"
            last_bar[depth+1] = "  "
        if isinstance(value, parser.ASTNode):
            print_node(value, depth+1, key, last_char, last_bar.copy(), show_private=show_private)
        else:
            print("".join(last_bar[:-1]) + last_char + key + ": " + repr(value))

