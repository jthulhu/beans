#!/usr/bin/python3

from beansast import ASTBuilder, parser
import sys, textwrap

p = ASTBuilder()

def ret_tree(tree):
    preconf = textwrap.dedent("""\
    node [shape="plaintext"];
    node0 [label="AST"];""")
    vertexes, edges = ret_node(tree, cur="0", first=True)
    content = """\
// Config
{preconf}

// Nodes definition
{nodes}

// Edges definition
{edges}
""".format(preconf=preconf, nodes='\n'.join(vertexes), edges='\n'.join(edges))
    text = 'digraph G {\n' + textwrap.indent(content, '  ') + "}\n"
    return text

def ret_node(node, cur, first=False):
    vertex, edges = [], []
    for i, (key, value) in enumerate(node.attributes.items()):
        i_cur = '%s_%s' % (cur, i)
        vertex.append('node%s [label="%s"];' % (i_cur, key))
        edges.append('node%s -> node%s;' % (cur, i_cur))
        if isinstance(value, parser.ASTNode):
            v, e = ret_node(value, i_cur)
        else:
            v = ['node%s_1 [label="%s"];' % (i_cur, value)]
            e = ['node%s -> node%s_1;' % (i_cur, i_cur)]
        vertex.extend(v)
        edges.extend(e)
    return vertex, edges

if __name__ == "__main__":
    with open(sys.argv[1]) as f:
        ast = p.get_ast(f.read())
    with open(sys.argv[2], 'w') as f:
        f.write(ret_tree(ast))
