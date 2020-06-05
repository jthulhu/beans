import os

BASEDIR = os.path.dirname(os.path.abspath(__file__))
GMRDIR = os.path.join(BASEDIR, 'gmrs')
LEXER_GRAMMAR = os.path.join(GMRDIR, 'lexer.gmr')
PARSER_GRAMMAR = os.path.join(GMRDIR, 'parser.gmr')
PLEXER_GRAMMAR = os.path.join(GMRDIR, 'plexer.gmr')

def locate_path(*path):
    return os.path.join(BASEDIR, *path)
