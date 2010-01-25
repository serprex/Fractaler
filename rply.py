#!/usr/bin/env python
from random import random,randint
from sys import argv
mxval=int(argv[1])*2 if len(argv)>1 else 6
mxcof=int(argv[2]) if len(argv)>2 else 5
def rand(x):return x*(random()-.5)
try:
	while 1:print ",".join(repr(rand(mxval))+"+"+repr(rand(mxval)) for x in range(randint(1,mxcof)))
except:pass
