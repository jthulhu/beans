import types
from .primitives import primitives

class Token:
    def __init__(self, primitive, value=None):
        self.code = primitive["code"]
        self.name = primitive["name"]
        self.repr = primitive["repr"]
        self.value = value
    def __repr__(self):
        return '<Token %s - %s>' % (self.name, self.repr)


class RawParser:
    def __init__(self, flux, pos):
        self.flux = flux
        self.pos = pos        # this one increments while parsing
                              # and is returned in success
        self.origin_pos = pos # this one is returned in case of fail
        self._post_init()
    def _post_init(self):
        pass
    def one_character(self, character, token):
        self.character = character
        self.token = token
        def check_first(self):
            return self.flux[self.pos] == self.character
        def check_all(self):
            return (True, self.pos + 1, token)
        self.check_first = types.MethodType(check_first, self)
        self.check_all = types.MethodType(check_all, self)
    def static_word(self, word, token):
        self.word = word
        self.token = token
        def check_first(self):
            return self.flux[self.pos] == self.word[0]
        def check_all(self):
            wpos = 0
            while self.pos < len(self.flux):
                if wpos + 1 == len(self.word):
                    return (True, self.pos + 1, self.token)
                if self.flux[self.pos] != self.word[wpos]:
                    return (False, self.origin_pos, None)
                wpos += 1
                self.pos += 1
            return (False, self.origin_pos, None)
        self.check_first = types.MethodType(check_first, self)
        self.check_all = types.MethodType(check_all, self)
    def is_letter(self, a):
        return a >= "a" and a <= "z" or a >= "A" and a <="Z"
    def is_number(self, a):
        return a >= "0" and a <= "9"
    def next_char(self):
        self.pos += 1
        return self.flux[self.pos - 1]

class EOLParser(RawParser):
    def _post_init(self):
        self.one_character("\n", None)
    
class DataParser(RawParser):
    def _post_init(self):
        self.static_word("data", Token(primitives["DATA"]))
    
class OrParser(RawParser):
    def _post_init(self):
        self.static_word("or", Token(primitives["OR"]))
    
class NotParser(RawParser):
    def _post_init(self):
        self.static_word("not", Token(primitives["NOT"]))
    
class IfParser(RawParser):
    def _post_init(self):
        self.static_word("if", Token(primitives["IF"]))
    
class ImportParser(RawParser):
    def _post_init(self):
        self.static_word("import", Token(primitives["IMPORT"]))
    
class ForParser(RawParser):
    def _post_init(self):
        self.static_word("for", Token(primitives["FOR"]))
    
class FromParser(RawParser):
    def _post_init(self):
        self.static_word("from", Token(primitives["FROM"]))
    
class ElifParser(RawParser):
    def _post_init(self):
        self.static_word("elif", Token(primitives["ELIF"]))
    
class ModuleParser(RawParser):
    def _post_init(self):
        self.static_word("module", Token(primitives["MODULE"]))
    
class PackageParser(RawParser):
    def _post_init(self):
        self.static_word("package", Token(primitives["PACKAGE"]))
    
class PrivateParser(RawParser):
    def _post_init(self):
        self.static_word("private", Token(primitives["PRIVATE"]))
    
class ElseParser(RawParser):
    def _post_init(self):
        self.static_word("else", Token(primitives["ELSE"]))
    
class SpacenameParser(RawParser):
    def _post_init(self):
        self.static_word("spacename", Token(primitives["SPACENAME"]))
    
class AndParser(RawParser):
    def _post_init(self):
        self.static_word("and", Token(primitives["AND"]))
    
class MethodParser(RawParser):
    def _post_init(self):
        self.static_word("method", Token(primitives["METHOD"]))
    
class PublicParser(RawParser):
    def _post_init(self):
        self.static_word("public", Token(primitives["PUBLIC"]))
    
class ClassParser(RawParser):
    def _post_init(self):
        self.static_word("class", Token(primitives["CLASS"]))
    
class FunctionParser(RawParser):
    def _post_init(self):
        self.static_word('function', Token(primitives["FUNCTION"]))
    
class WhileParser(RawParser):
    def _post_init(self):
        self.static_word('while', Token(primitives["WHILE"]))
    
class DotParser(RawParser):
    def _post_init(self):
        self.one_character(".", Token(primitives["DOT"]))
    
class AsteriskParser(RawParser):
    def _post_init(self):
        self.one_character("*", Token(primitives["ASTERISK"]))
    
class LessParser(RawParser):
    def _post_init(self):
        self.one_character("<", Token(primitives["LESS"]))

class TildeParser(RawParser):
    def _post_init(self):
        self.one_character("~", Token(primitives["TILDE"]))
        
class DoubleEqualsParser(RawParser):
    def check_first(self):
        return self.flux[self.pos] == "="
    def check_all(self):
        if self.pos + 1 < len(self.flux) and self.flux[self.pos + 1] == "=":
            return (True, self.pos + 2, Token(primitives["DOUBLE_EQUALS"]))
        return (False, self.pos, None)
    
class PlusParser(RawParser):
    def _post_init(self):
        self.one_character("+", Token(primitives["PLUS"]))
    
class MinusParser(RawParser):
    def _post_init(self):
        self.one_character("-", Token(primitives["MINUS"]))
    
class NotEqualsParser(RawParser):
    def check_first(self):
        return self.flux[self.pos] == "!"
    def check_all(self):
        if self.pos + 1 < len(self.flux) and self.flux[self.pos + 1] == "=":
            return (True, self.pos + 2, Token(primitives["NOT_EQUAL"]))
        return (False, self.pos, None)
    
class CloseBraceParser(RawParser):
    def _post_init(self):
        self.one_character('}', Token(primitives["RBRACE"]))
    
class GraveParser(RawParser):
    """
not usefull yet,
dont know why I implemented it
"""

    def _post_init(self):
        self.one_character('`', Token(primitives["GRAVE"]))
    
class FloatParser(RawParser):
    def check_first(self):
        return self.is_number(self.flux[self.pos])
    def check_all(self):
        result = self.next_char()
        while self.pos < len(self.flux) and self.is_number(self.flux[self.pos]):
            result += self.next_char()
        if self.pos < len(self.flux) and self.flux[self.pos] == ".":
            result += self.next_char()
        else:
            return (False, self.origin_pos, None)
        if not self.is_number(self.flux[self.pos]): return (False, self.origin_pos, None)
        while self.pos < len(self.flux) and self.is_number(self.flux[self.pos]):
            result += self.next_char()

        return (True, self.pos, Token(primitives["FLOAT"], result))
    
class MoreParser(RawParser):
    def _post_init(self):
        self.one_character('>', Token(primitives["MORE"]))
    
class TabulationParser(RawParser):
    def _post_init(self):
        self.one_character("\t", None)
    
class OpenBraceParser(RawParser):
    def _post_init(self):
        self.one_character("{", Token(primitives["LBRACE"]))
    
class CloseBracketParser(RawParser):
    def _post_init(self):
        self.one_character("]", Token(primitives["RBRACKET"]))
    
class MoreOrEqualParser(RawParser):
    def check_first(self):
        return self.flux[self.pos] == ">"
    def check_all(self):
        if self.pos + 1 < len(self.flux) and self.flux[self.pos + 1] == '=':
            return (True, self.pos + 2, Token(primitives["MORE_OR_EQUAL"]))
        return (False, self.pos, None)
    
class IntParser(RawParser):
    def check_first(self):
        return self.is_number(self.flux[self.pos])
    def check_all(self):
        result = self.next_char()
        while self.pos < len(self.flux) and self.is_number(self.flux[self.pos]):
            result += self.next_char()
        return (True, self.pos, Token(primitives["INT"], result))
    
class OpenBracketParser(RawParser):
    def _post_init(self):
        self.one_character("[", Token(primitives["LBRACKET"]))
    
class LessOrEqualParser(RawParser):
    def check_first(self):
        return self.flux[self.pos] == "<"
    def check_all(self):
        if self.pos + 1 < self.flux[self.pos] and self.flux[self.pos + 1] == "=":
            return (True, self.pos + 2, Token(primitives["LESS_OR_EQUAL"]))
        return (False, self.pos, None)
    
class SlashParser(RawParser):
    def _post_init(self):
        self.one_character('/', Token(primitives["SLASH"]))
    
class DoubleGraveParser(RawParser):
    """
not usefull yet,
dont know why I implemented it
"""

    def check_first(self):
        return self.flux[self.pos] == "`"
    def check_all(self):
        if self.pos + 1 < len(self.flux) and self.flux[self.pos + 1] == '`':
            return (True, self.pos + 2, Token(primitives["DOUBLE_GRAVE"]))
        return (False, self.pos, None)
    
class CommaParser(RawParser):
    def _post_init(self):
        self.one_character(",", Token(primitives["COMMA"]))
    
class CloseParentheseParser(RawParser):
    def _post_init(self):
        self.one_character(")", Token(primitives["RPAR"]))
    
class OpenParentheseParser(RawParser):
    def _post_init(self):
        self.one_character("(", Token(primitives["LPAR"]))
    
class ColonParser(RawParser):
    def _post_init(self):
        self.one_character(":", Token(primitives["COLON"]))
    
class SemicolonParser(RawParser):
    def _post_init(self):
        self.one_character(";", Token(primitives["SEMICOLON"]))
    
class EqualsParser(RawParser):
        def _post_init(self):
            self.one_character('=', Token(primitives["EQUALS"]))

class StringParser(RawParser):
    def check_first(self):
        return self.flux[self.pos] in {"'", '"'}
    def check_all(self):
        delimiter = self.next_char()
        result = ""
        ignore = False
        while self.pos < len(self.flux):
            if ignore:
                result += self.next_char()
                ignore = False
            elif self.flux[self.pos] == delimiter:
                return (True, self.pos + 1, Token(primitives["STRING"], result))
            elif self.flux[self.pos] == "\\":
                ignore = True
            else:
                result += self.next_char()
        return (False, self.origin_pos, None)


class EOFParser(RawParser):
    def check_first(self):
        return self.pos == len(self.flux)
    def check_all(self):
        return (True, self.pos, Token(primitives["EOF"]))
    
class VariableParser(RawParser):
    def check_first(self):
        return self.is_letter(self.flux[self.pos]) or self.flux[self.pos] == '_'
    def check_all(self):
        result = self.next_char()
        while self.pos < len(self.flux) and (self.is_letter(self.flux[self.pos]) or self.flux[self.pos] == '_'):
            result += self.next_char()
        return (True, self.pos, Token(primitives["VARIABLE"], result))

class SpaceParser(RawParser):
    def _post_init(self):
        self.one_character(' ', None)
    
raw_parsers = (
    EOFParser,
    SpaceParser,
    EOLParser,
    DataParser,
    OrParser,
    NotParser,
    IfParser,
    ImportParser,
    ForParser,
    FromParser,
    ElifParser,
    ModuleParser,
    PackageParser,
    PrivateParser,
    ElseParser,
    SpacenameParser,
    AndParser,
    MethodParser,
    PublicParser,
    ClassParser,
    FunctionParser,
    WhileParser,
    DotParser,
    AsteriskParser,
    LessParser,
    DoubleEqualsParser,
    PlusParser,
    MinusParser,
    NotEqualsParser,
    CloseBraceParser,
    FloatParser,
    MoreParser,
    TabulationParser,
    OpenBraceParser,
    CloseBracketParser,
    MoreOrEqualParser,
    IntParser,
    OpenBracketParser,
    LessOrEqualParser,
    SlashParser,
    DoubleGraveParser,
    GraveParser,
    CommaParser,
    CloseParentheseParser,
    OpenParentheseParser,
    ColonParser,
    SemicolonParser,
    EqualsParser,
    StringParser,
    VariableParser
)