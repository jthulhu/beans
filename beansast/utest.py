from unittest import TestCase
import unittest

# tested libraries
from . import lxrgmrparser, lexer, psrgmrparser, parser, ShellAST

# libraries used to recreate result of test, to be compared with
# the actual result of test
import re
from collections import OrderedDict

class TestLxrgmrparser(TestCase):
    """TestCase to test all the lxrgmrparser functionalities"""
    def test_token(self):
        """test lxrgmrparser.Token"""
        # building tokens
        t1 = lxrgmrparser.Token("ID", {"name": "a"})
        t2 = lxrgmrparser.Token("DOT", {})

        # test token definition
        self.assertTrue(hasattr(t1, "name") and t1.name == "ID" and hasattr(t1, "attributes") and t1.attributes == {"name": "a"} and t1.attributes["name"] == "a")
        self.assertTrue(hasattr(t2, "name") and t2.name == "DOT" and hasattr(t2, "attributes") and t2.attributes == {})
        
    def test_tokenizer(self):
        """test lxrgmrparser.Tokenizer"""
        # note that lxrgmrparser.Tokenizer requires lxrgmrparser.Token
        # errors might come from it if you didn't test it yet
        
        # building tokenizers
        tr1 = lxrgmrparser.Tokenizer("SPACE", "[\t ]+", True)
        tr1b1 = lxrgmrparser.Tokenizer("SPACE", "[\t ]+")
        tr1b2 = lxrgmrparser.Tokenizer("SPACE", "[\t ]+", True)
        tr2 = lxrgmrparser.Tokenizer("NEWLINE", "\n", True)
        tr3 = lxrgmrparser.Tokenizer("INT", "(?P<value>\d+)")
        tr3b = lxrgmrparser.Tokenizer("INT", "\d+")

        # test tokenizer definition
        self.assertTrue(hasattr(tr1, "name") and tr1.name == "SPACE" and hasattr(tr1, "rule") and tr1.rule == re.compile("[\t ]+", re.M) and hasattr(tr1, "ignore") and tr1.ignore)
        self.assertTrue(hasattr(tr1b1, "name") and tr1b1.name == "SPACE" and hasattr(tr1b1, "rule") and tr1b1.rule == re.compile("[\t ]+", re.M) and hasattr(tr1b1, "ignore") and not tr1b1.ignore)
        self.assertFalse(tr1 == tr1b1)
        self.assertTrue(hasattr(tr1b2, "name") and tr1b2.name == "SPACE" and hasattr(tr1b2, "rule") and tr1b2.rule == re.compile("[\t ]+", re.M) and hasattr(tr1b2, "ignore") and tr1b2.ignore)
        self.assertTrue(tr1 == tr1b2)
        self.assertTrue(hasattr(tr2, "name") and tr2.name == "NEWLINE" and hasattr(tr2, "rule") and tr2.rule == re.compile("\n", re.M) and hasattr(tr2, "ignore") and tr2.ignore)
        self.assertTrue(hasattr(tr3, "name") and tr3.name == "INT" and hasattr(tr3, "rule") and tr3.rule == re.compile("(?P<value>\d+)", re.M) and hasattr(tr3, "ignore") and not tr3.ignore)
        self.assertTrue(hasattr(tr3b, "name") and tr3b.name == "INT" and hasattr(tr3b, "rule") and tr3b.rule == re.compile("\d+", re.M) and hasattr(tr3b, "ignore") and not tr3b.ignore)
        self.assertFalse(tr3 == tr3b)

        # test match function
        self.assertTrue(tr1("ab  \t ", 2) == (True, 6, None))
        self.assertTrue(tr1("ab  \t ", 3) == (True, 6, None))
        self.assertTrue(tr1("ab  \t ", 1) == (False, 1, None))
        self.assertTrue(tr1b1("abc ", 3) == (True, 4, lxrgmrparser.Token("SPACE", {})))
        # tr1b2 == tr1, no tests needed
        self.assertTrue(tr2("\n\n", 0) == (True, 1, None))
        self.assertTrue(tr3("34721", 0) == (True, 5, lxrgmrparser.Token("INT", {"value": "34721"})))
        self.assertTrue(tr3("34.721", 0) == (True, 2, lxrgmrparser.Token("INT", {"value": "34"})))
        self.assertTrue(tr3b("34721", 0) == (True, 5, lxrgmrparser.Token("INT", {})))
        
        
    def test_lexerreader(self):
        """test lxrgmrparser.LexerReader"""
        # note that lxrgmrparser.LexerReader requires
        # lxrgmrparser.Tokenizer to work, errors might come from it
        # if you didn't test it yet
        
        with open("beansast/tests/test_lxrgmrparser_lexerreader.gmr") as f:
            tgrammar = f.read()
        lr = lxrgmrparser.LexerReader(tgrammar)

        # build excpected ordered dict
        t = lxrgmrparser.Tokenizer
        od = OrderedDict((
            ('TABULATION', t("TABULATION", r'\t')),
            ('SPACE', t('SPACE', r'\s', True)),
            ('NEWLINE', t('NEWLINE', r'\n', True)),
            ('DOT', t('DOT', r'[.]')),
            ('ID', t('ID', r'(?P<name>\w+)')),
            ('STRING', t('STRING', r'("|' "'" r')(?P<value>(([^\1\\]|((\\\\)*\\[^\\])|(\\\\)*\\\1)*)((\\\\)|(\\)?[^\\]|))?\1')),
            ('INT', t('INT', r'(?P<value>\d+)', True))
        ))
        a = lr.read()

        # testing lexer reader
        self.assertEqual(a[1], [])
        self.assertEqual(a[0].keys(), od.keys())
        for key, name in a[0].items():
            self.assertEqual(name, od[key])
            
    def test_lexerreader_update(self):
        """test lxrgmrparser.LexerReader().update"""
        # building lexer reader
        with open("beansast/tests/test_lxrgmrparser_lexerreader_update.gmr") as f:
            tgrammar = f.read()
        lr = lxrgmrparser.LexerReader(tgrammar)
        t = lxrgmrparser.Tokenizer
        del_ = ["TABULATION", "DOT"]
        od = OrderedDict((
            ('SPACE', t('SPACE', r'[\t ]', True)),
            ('NEWLINE', t('NEWLINE', r'\n', True)),
            ('STRING', t('STRING', r'("|' "'" r')(?P<value>(([^\1\\]|((\\\\)*\\[^\\])|(\\\\)*\\\1)*)((\\\\)|(\\)?[^\\]|))?\1')),
            ('INT', t('INT', r'(?P<value>\d+)'))
        ))
        a = lr.read()

        # testing lexer reader
        self.assertEqual(a[1], del_)
        self.assertEqual(a[0].keys(), od.keys())
        for key, name in a[0].items():
            self.assertEqual(name, od[key])

class TestLexer(TestCase):
    def test_lexer(self):
        # lexer module has only one feature -- it's its class Lexer
        # so this test case only has one test

        # remember lexer.Lexer engine is built uppon
        # lxrgmrparser.LexerReader, so errors might come from it
        # if you didn't test it before

        # this test also includes string limit cases to test
        # the grammar, whose keys have been taken from
        # beansast/gmrs/lexer.gmr
        # in particular strings, which have been a continues
        # source of bugs (look at the regex)
        
        # building lexer
        with open("beansast/tests/test_lexer_lexer.gmr") as f:
            l = lexer.Lexer(f.read())
          
        with open("beansast/tests/test_lexer_lexer.bns") as f:
            l(f.read())

        # building test tokenizers
        with open("beansast/tests/test_lexer_lexer.gmr") as f:
            tr = lxrgmrparser.LexerReader(f.read()).read()

        # building tokens
        t = lxrgmrparser.Token
        tokens = [
            t("ID", {"name": "Az"}),
            t("STRING", {"value": "wow'"}),
            t("ID", {"name": "ZDHIZ"}),
            t("STRING", {"value": "\\'"}),
            t("STRING", {"value": "\\\\"}),
            t("STRING", {"value": ""}),
            t("STRING", {"value": 'a'}),
            t("EQUAL", {}),
            t("INT", {"value": "6"}),
            t("INT", {"value": "5"}),
            t("ID", {"name": "53Ie3"}),
            t("ID", {"name": "L"}),
            t("EOF", {})
        ]
        
        # test lexer.tokenizers instaciated well
        self.assertEqual(l.tokenizers, tr[0])
        i = 0
        while l[i] != tokens[-1]:
            self.assertLess(i, len(tokens))
            self.assertEqual(l[i], tokens[i])
            i += 1
        
        

class TestPsrgmrparser(TestCase):
    pass

class TestParser(TestCase):
    pass

class TestExpressions(TestCase):
    def test_raw_exprs(self):
        with open("beansast/tests/test_exprs.bns") as f:
            instructions = f.readlines()
        shell = ShellAST
        node = psrgmrparser.ASTNode
        d = OrderedDict
        results = [
            [node("Statement", d((("op", "builtin"), ("value", "a"), ("opprior", 1), ("type", "string"), ("s", "expr"))), "TEST", "TEST")],
            [node("Statement", d((("op", "builtin"), ("value", "a"), ("opprior", 1), ("type", "id"), ("s", "expr"))), "TEST", "TEST")],
            [node("Statement", d((("op", "builtin"), ("value", "a"), ("opprior", 1), ("type", "id"), ("s", "expr"))), "TEST", "TEST")],
            [node("Statement", d((("op", "builtin"), ("value", "6"), ("opprior", 1), ("type", "int"), ("s", "expr"))), "TEST", "TEST")],
            [node("Statement", d((("op", "builtin"), ("value", "6"), ("opprior", 1), ("type", "float"), ("s", "expr"))), "TEST", "TEST")],
            [node("Statement", d((("op", "builtin"), ("value", ".6"), ("opprior", 1), ("type", "float"), ("s", "expr"))), "TEST", "TEST")],
            [node("Statement", d((("op", "builtin"), ("value", "6.6"), ("opprior", 1), ("type", "float"), ("s", "expr"))), "TEST", "TEST")],
            [node("Statement", d((("op", "builtin"), ("opprior", 1), ("type", "true"), ("s", "expr"))), "TEST", "TEST")],
            [node("Statement", d((("op", "builtin"), ("opprior", 1), ("type", "false"), ("s", "expr"))), "TEST", "TEST")],
            [node("Statement", d((("op", "not"),
                                  ("value",
                                   node("Expression", d((("op", "builtin"), ("value", "a"), ("opprior", 1), ("type", "id"))))
                                  ),
                                  ("opprior", 1), ("type", "string"), ("s", "expr"))), "TEST", "TEST")],
            [node("Statement", d((("op", "not"),
                                  ("value",
                                   node("Expression", d((("op", "builtin"), ("value", "a"), ("opprior", 1), ("type", "id"))))
                                  ),
                                  ("opprior", 1), ("type", "string"), ("s", "expr"))), "TEST", "TEST")],
            [node("Statement", d((("op", "and"), ("value", "computation"), ("opprior", 1)
                                  ("left",
                                   node("Expression", d((("op", "builtin"), ("value", "a"), ("opprior", 1), ("type", "id")))),
                                   ("right",
                                   node("Expression", d((("op", "builtin"), ("value", "b"), ("opprior", 1), ("type", "id"))))
                                  ),
                                  ("type", "string"), ("s", "expr"))), "TEST", "TEST"))],
             [node("Statement", d((("op", "or"), ("value", "computation"), ("opprior", 1)
                                  ("left",
                                   node("Expression", d((("op", "builtin"), ("value", "a"), ("opprior", 1), ("type", "id")))),
                                   ("right",
                                   node("Expression", d((("op", "builtin"), ("value", "b"), ("opprior", 1), ("type", "id"))))
                                  ),
                                   ("type", "string"), ("s", "expr"))), "TEST", "TEST"))],
              [node("Statement", d((("op", "xor"), ("value", "computation"), ("opprior", 1)
                                  ("left",
                                   node("Expression", d((("op", "builtin"), ("value", "a"), ("opprior", 1), ("type", "id")))),
                                   ("right",
                                   node("Expression", d((("op", "builtin"), ("value", "b"), ("opprior", 1), ("type", "id"))))
                                  ),
                                   ("type", "string"), ("s", "expr"))), "TEST", "TEST"))],
               [node("Statement", d((("op", "eq"), ("value", "computation"), ("opprior", 1)
                                  ("left",
                                   node("Expression", d((("op", "builtin"), ("value", "a"), ("opprior", 1), ("type", "id")))),
                                   ("right",
                                   node("Expression", d((("op", "builtin"), ("value", "b"), ("opprior", 1), ("type", "id"))))
                                  ),
                                   ("type", "string"), ("s", "expr"))), "TEST", "TEST"))],
                [node("Statement", d((("op", "ne"), ("value", "computation"), ("opprior", 1)
                                  ("left",
                                   node("Expression", d((("op", "builtin"), ("value", "a"), ("opprior", 1), ("type", "id")))),
                                   ("right",
                                   node("Expression", d((("op", "builtin"), ("value", "b"), ("opprior", 1), ("type", "id"))))
                                  ),
                                   ("type", "string"), ("s", "expr"))), "TEST", "TEST"))],
                 [node("Statement", d((("op", "is"), ("value", "computation"), ("opprior", 1)
                                  ("left",
                                   node("Expression", d((("op", "builtin"), ("value", "a"), ("opprior", 1), ("type", "id")))),
                                   ("right",
                                   node("Expression", d((("op", "builtin"), ("value", "b"), ("opprior", 1), ("type", "id"))))
                                  ),
                                   ("type", "string"), ("s", "expr"))), "TEST", "TEST"))],
                  [node("Statement", d((("op", "has"), ("value", "computation"), ("opprior", 1)
                                  ("left",
                                   node("Expression", d((("op", "builtin"), ("value", "a"), ("opprior", 1), ("type", "id")))),
                                   ("right",
                                   node("Expression", d((("op", "builtin"), ("value", "b"), ("opprior", 1), ("type", "id"))))
                                  ),
                                  ("type", "string"), ("s", "expr"))), "TEST", "TEST"))],
                   [node("Statement", d((("op", "in"), ("value", "computation"), ("opprior", 1)
                                  ("left",
                                   node("Expression", d((("op", "builtin"), ("value", "a"), ("opprior", 1), ("type", "id")))),
                                   ("right",
                                   node("Expression", d((("op", "builtin"), ("value", "b"), ("opprior", 1), ("type", "id"))))
                                  ),
                                  ("type", "string"), ("s", "expr"))), "TEST", "TEST"))],
                    [node("Statement", d((("op", "gt"), ("value", "computation"), ("opprior", 1)
                                  ("left",
                                   node("Expression", d((("op", "builtin"), ("value", "a"), ("opprior", 1), ("type", "id")))),
                                   ("right",
                                   node("Expression", d((("op", "builtin"), ("value", "b"), ("opprior", 1), ("type", "id"))))
                                  ),
                                  ("type", "string"), ("s", "expr"))), "TEST", "TEST"))],
                     [node("Statement", d((("op", "ge"), ("value", "computation"), ("opprior", 1)
                                  ("left",
                                   node("Expression", d((("op", "builtin"), ("value", "a"), ("opprior", 1), ("type", "id")))),
                                   ("right",
                                   node("Expression", d((("op", "builtin"), ("value", "b"), ("opprior", 1), ("type", "id"))))
                                  ),
                                  ("type", "string"), ("s", "expr"))), "TEST", "TEST"))],
                      [node("Statement", d((("op", "lt"), ("value", "computation"), ("opprior", 1)
                                  ("left",
                                   node("Expression", d((("op", "builtin"), ("value", "a"), ("opprior", 1), ("type", "id")))),
                                   ("right",
                                   node("Expression", d((("op", "builtin"), ("value", "b"), ("opprior", 1), ("type", "id"))))
                                  ),
                                  ("type", "string"), ("s", "expr"))), "TEST", "TEST"))],
                       [node("Statement", d((("op", "le"), ("value", "computation"), ("opprior", 1)
                                  ("left",
                                   node("Expression", d((("op", "builtin"), ("value", "a"), ("opprior", 1), ("type", "id")))),
                                   ("right",
                                   node("Expression", d((("op", "builtin"), ("value", "b"), ("opprior", 1), ("type", "id"))))
                                  ),
                                  ("type", "string"), ("s", "expr"))), "TEST", "TEST"))],
                        [node("Statement", d((("op", "dot"), ("value", "computation"), ("opprior", 1)
                                  ("left",
                                   node("Expression", d((("op", "builtin"), ("value", "a"), ("opprior", 1), ("type", "id")))),
                                   ("right",
                                   node("Expression", d((("op", "builtin"), ("value", "b"), ("opprior", 1), ("type", "id"))))
                                  ),
                                  ("type", "string"), ("s", "expr"))), "TEST", "TEST"))],
                         [node("Statement", d((("op", "call"),
                                  ("value",
                                   node("Expression", d((("op", "builtin"), ("value", "a"), ("opprior", 1), ("type", "id"))))
                                  ),
                                               ("opprior", 1), ("s", "expr"))), "TEST", "TEST")],
                      [node("Statement", d((("op", "pow"), ("value", "computation"), ("opprior", 1)
                                  ("left",
                                   node("Expression", d((("op", "builtin"), ("value", "a"), ("opprior", 1), ("type", "id")))),
                                   ("right",
                                   node("Expression", d((("op", "builtin"), ("value", "b"), ("opprior", 1), ("type", "id"))))
                                  ),
                                   ("type", "string"), ("s", "expr"))), "TEST", "TEST"))],
                       [node("Statement", d((("op", "plus"),
                                  ("value",
                                   node("Expression", d((("op", "builtin"), ("value", "a"), ("opprior", 1), ("type", "id"))))
                                  ),
                                  ("opprior", 1), ("s", "expr"))), "TEST", "TEST")],
                       [node("Statement", d((("op", "minus"),
                                  ("value",
                                   node("Expression", d((("op", "builtin"), ("value", "a"), ("opprior", 1), ("type", "id"))))
                                  ),
                                  ("opprior", 1), ("s", "expr"))), "TEST", "TEST")],
                       [node("Statement", d((("op", "tilde"),
                                  ("value",
                                   node("Expression", d((("op", "builtin"), ("value", "a"), ("opprior", 1), ("type", "id"))))
                                  ),
                                  ("opprior", 1), ("s", "expr"))), "TEST", "TEST")],
                       [node("Statement", d((("op", "dplus"),
                                  ("value",
                                   node("Expression", d((("op", "builtin"), ("value", "a"), ("opprior", 1), ("type", "id"))))
                                  ),
                                  ("opprior", 1), ("s", "expr"))), "TEST", "TEST")],
                       [node("Statement", d((("op", "dminus"),
                                  ("value",
                                   node("Expression", d((("op", "builtin"), ("value", "a"), ("opprior", 1), ("type", "id"))))
                                  ),
                                  ("opprior", 1), ("s", "expr"))), "TEST", "TEST")],
                       [node("Statement", d((("op", "dplus"),
                                  ("value",
                                   node("Expression", d((("op", "builtin"), ("value", "a"), ("opprior", 1), ("type", "id"))))
                                  ),
                                  ("opprior", 2), ("s", "expr"))), "TEST", "TEST")],
                       [node("Statement", d((("op", "dminus"),
                                  ("value",
                                   node("Expression", d((("op", "builtin"), ("value", "a"), ("opprior", 1), ("type", "id"))))
                                  ),
                                  ("opprior", 2), ("s", "expr"))), "TEST", "TEST")],
                       [node("Statement", d((("op", "mul"), ("value", "computation"), ("opprior", 1)
                                  ("left",
                                   node("Expression", d((("op", "builtin"), ("value", "a"), ("opprior", 1), ("type", "id")))),
                                   ("right",
                                   node("Expression", d((("op", "builtin"), ("value", "b"), ("opprior", 1), ("type", "id"))))
                                  ),
                                   ("type", "string"), ("s", "expr"))), "TEST", "TEST"))],
                        # 33
                       
        ]
            
        

tests = unittest.TestSuite()
tests.addTest(TestLxrgmrparser("test_token"))
tests.addTest(TestLxrgmrparser("test_tokenizer"))
tests.addTest(TestLxrgmrparser("test_lexerreader"))
tests.addTest(TestLxrgmrparser("test_lexerreader_update"))
tests.addTest(TestLexer("test_lexer"))

if __name__ == '__main__':
    raise NotImplementedError
