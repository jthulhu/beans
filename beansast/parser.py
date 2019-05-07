from .psrgmrparser import ParserReader
from .lexer import Lexer

class ParserError(BaseException):
    pass

class Parser:
    def __init__(self, rgrammar, fn="<stdin>", helperr=False):
        self.helperr = helperr
        with open("beansast/gmrs/plexer-m.gmr") as f:
            grammar = Lexer(f.read(), file="beansast/gmrs/plexer.gmr", helperr=helperr)
        grammar(rgrammar, fn=fn)
        self.metastmts, self.nodizers = ParserReader(grammar, helperr=helperr).read()
    def __call__(self, flux, pos=0):
        self.flux = flux
        self.file = flux.fn
        self.pos = pos
        self.stack = []
        self.time = pos + 1
    def parse(self):
        
        for nodizer in self.nodizers.values():
            is_good, pos, result = nodizer(self.flux, self.pos).check(self.results)
            if is_good:
                self.pos = pos
                break
        while is_good:
            for nodizer in self.nodizers.items():
                is_good, pos, result = nodizer(self.flux, self.pos).check(self.results)
                if is_good:
                    self.pos = pos
                    break
        return self.results, self.pos
    def update(self, grammar):
        nmetastmts, nnodizers = ParserReader(grammar, helperr=helperr).read()
        self.nodizers.update(nnodizers)
    def __eq__(self, r):
        return type(self) == type(r) and hasattr(r, "nodizers") and r.nodizers == self.nodizers and hasattr(r, "pos") and r.pos == self.pos and hasattr(r, "flux") and self.flux == r.flux
    def __repr__(self):
        fn = "" if not hasattr(self, "file") else (" instancied of file %s" % self.file)
        return "<Parser%s at %s>" % (fn, hex(id(self)))
            
