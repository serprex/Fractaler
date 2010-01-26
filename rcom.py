#!/usr/bin/env python
from random import random
from sys import argv
rng=int(argv[1])*2 if len(argv)>1 else 2
def rand():return rng*(random()-.5)
try:
	while 1:print(repr(rand())+"+"+repr(rand()))
except:pass
