from .psrgmrparser import ParserReader
from .lexer import Lexer

class ParserError(Exception):
    pass

class Parser:
    def __init__(self, rgrammar):
        with open("beansast/gmrs/plexer.gmr") as f:
            grammar = Lexer(f.read())
        grammar(rgrammar)
        self.nodizers = ParserReader(grammar).read()
    def __call__(self, flux, pos=0):
        self.flux = flux
        self.pos = pos
    def parse(self, context):
        for nodizer in self.nodizers:
            is_good, pos, result = nodizer(self.flux, self.pos).check(context)
            if is_good:
                self.pos = pos
                return is_good, result
        return False, None
    def update(self, grammar):
        self.nodizers.update(ParserReader(grammar).read())
    def __eq__(self, r):
        return type(self) == type(r) and hasattr(r, "nodizers") and r.nodizers == self.nodizers and hasattr(r, "pos") and r.pos == self.pos and hasattr(r, "flux") and self.flux == r.flux
    def __repr__(self):
        return "<Parser%s at %s>" % ((" instancied") * int(hasattr(self, "flux")), str(id(self)))
            
