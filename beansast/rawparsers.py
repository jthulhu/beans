import types
from .primitives import primitives
class Token:
    def __init__(self, primitive, value=None):
        self.id = primitives[primitive]
        self.repr = primitive
        self.value = value
    def __repr__(self):
        return '<Token %s%s>' % (self.repr, (" - %s" % self.value) * int(self.value != None) )
    


class RawParser:
    def __init__(self, flux, pos):
        self.flux = flux
        self.pos = pos        # this one increments while parsing
                              # and is returned in success
        self.origin_pos = pos # this one is returned in case of fail
        self._post_init()
    def _post_init(self):
        pass
    def check_first(self):
        return False
    def check_all(self):
        return False
    def is_letter(self, a):
        return a != "EOF" and (a >= "a" and a <= "z" or a >= "A" and a <="Z")
    def is_number(self, a):
        return a != "EOF" and a >= "0" and a <= "9"
    def next_char(self):
        self.pos += 1
        return self.flux[self.pos - 1]

class IdParser(RawParser):
    def check_first(self):
        return self.is_letter(self.next_char())
    def check_all(self):
        result = ""
        next_ = self.next_char()
        while self.is_number(next_) or self.is_letter(next_):
            result += next_
            next_ = self.next_char()
        return (True, self.pos - 1, Token("id", result))

class EOFParser(RawParser):
    def check_first(self):
        return self.next_char() == "EOF"
    def check_all(self):
        return (True, self.pos + 1, Token("EOF"))
    
class StringParser(RawParser):
    def check_first(self):
        return self.next_char() in {'"', "'"}
    def check_all(self):
        delimiter = self.next_char()
        result = ""
        next_ = self.next_char()
        while next_ != delimiter:
            if next_ == "\\":
                result += self.next_char()
            else:
                result += next_
            next_ = self.next_char()
        return (True, self.pos, Token("string", result))

class IntParser(RawParser):
    def check_first(self):
        return self.is_number(self.next_char())
    def check_all(self):
        result = ""
        next_ = self.next_char()
        while self.is_number(next_):
            result += next_
            next_ = self.next_char()
        return (True, self.pos - 1, Token("int", int(result)))

class SpaceParser(RawParser):
    def check_first(self):
        return self.next_char() in {" ", '\t'}
    def check_all(self):
        result = 0
        next_ = self.next_char()
        while next_ in {" ", '\t'}:
            if next_ == " ":
                result += 1
            else:
                result += 4
            next_ = self.next_char()
        return (True, self.pos - 1, Token("space", result))

class EOLParser(RawParser):
    def check_first(self):
        return self.next_char() == "\n"
    def check_all(self):
        return (True, self.pos + 1, Token("EOL"))

class OperatorParser(RawParser):
    def check_first(self):
        next_ = self.next_char()
        return not (self.is_number(next_) or self.is_letter(next_) or next_ in {" ", "\t", "\n", "EOF"})
    def check_all(self):
        result = ""
        next_ = self.next_char()
        while not (self.is_number(next_) or self.is_letter(next_) or next_ in {" ", "\t", "\n", "EOF"}):
            result += next_
            next_ = self.next_char()
        return (True, self.pos - 1, Token("op", result))
    
raw_parsers = (
    IdParser,
    StringParser,
    IntParser,
    EOLParser,
    OperatorParser,
    SpaceParser,
    EOFParser
)
