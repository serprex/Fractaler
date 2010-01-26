#!/usr/bin/env python
from random import random,randint
from sys import argv
mxval=int(argv[1])*2 if len(argv)>1 else 10
mxcof=int(argv[2]) if len(argv)>2 else 6
mncof=int(argv[3]) if len(argv)>3 else 2
def rand():return mxval*(random()-.5)
try:
	while 1:print(",".join(repr(rand())+"+"+repr(rand()) for x in range(randint(mncof,mxcof))))
except:pass
