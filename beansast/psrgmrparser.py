#!/usr/bin/python3
# -*- coding: utf-8 -*-

class ASTNode:
    def __init__(self, name, attributes):
        self.name = name
        self.attributes = attributes
    def __repr__(self):
        return "<ASTNode named %s%s>" % (self.name, (" - %s" % str(self.attributes)) * int(bool(self.attributes)))

class ASTNodizer: # also called parser, but to not mess up with names
    def __init__(self, name, rule):
        self.name = name
        self.rule = rule
    def __call__(self, flux, pos):
        result = self.rule.match(flux[pos:])
