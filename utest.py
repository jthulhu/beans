#!/usr/bin/python3
# -*- coding: utf-8 -*-


import unittest
import beansast.utest

if __name__ == '__main__':
    runner = unittest.TextTestRunner()
    runner.run(beansast.utest.tests)
