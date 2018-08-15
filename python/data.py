#! /usr/bin/python3

import numpy as np
import matplotlib.pyplot as plt

# import data
mr = np.loadtxt('function.dat')

# reading two columns
plt.plot(mr[:,0], mr[:,1])

# setting horizontal axis
plt.xlim((0, 10))

# setting vertical axis
plt.ylim((0,100))

# label axis
plt.xlabel('x')
plt.ylabel('f(x)')

plt.savefig('graph.pdf')
plt.savefig('graph.eps')
plt.clf()
