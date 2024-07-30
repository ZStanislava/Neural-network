# -*- coding: utf-8 -*-
"""Detect_objects_1.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1rIfqqoBGMWyaDbtQm9sYtY77KKQUiZ61
"""

import math
import matplotlib.pyplot as plt
from matplotlib.ticker import MultipleLocator

X1 = ((1,0), (2,0),(1,1),(2,1),(2,2))
X2 = ( (5,1), (6,0), (6,1), (5,0), (6,2) )

def d(x,w):
  return x[0]*w[0] + x[1]*w[1] + 1*w[2]

w1 = len(X1)
w2 = len(X2)

z1 = [0,0]
for i in range(w1):
  z1[0] += 1/w1*X1[i][0]
  z1[1] += 1/w1*X1[i][1]
#z1[0] += 1/w1
#z1[1] += 1/w1
z2 = [0,0]
for i in range(w2):
  z2[0] += 1/w2*X2[i][0]
  z2[1] += 1/w2*X2[i][1]
#z2[0] += 1/w2
#z2[1] += 1/w2
w1 = [z1[0],z1[1],-1/2*(z1[0]*z1[0] + z1[1]*z1[1])]
w2 = [z2[0],z2[1],-1/2*(z2[0]*z2[0] + z2[1]*z2[1])]

x = []
x.append(float(input()))
x.append(float(input()))

d1 = d(x,w1)
d2 = d(x,w2)
if(d1 > d2):
  print("class1")
elif(d1 < d2):
  print(d2)
else:
  print("None")

def delta_d(x,z1,z2):
  y = (x*(z2[0] - z1[0]) + 1/2*(-1*(z2[0]*z2[0] + z2[1]*z2[1]) + (z1[0]*z1[0] + z1[1]*z1[1]) )) / (z1[1] - z2[1])
  
  return y

figure = plt.figure()
ax = figure.add_subplot()
ax.xaxis.set_major_locator(MultipleLocator(base=1))
ax.yaxis.set_major_locator(MultipleLocator(base=1))
ax.set_aspect('equal', adjustable='box')
x_grid = []
y_grid = []

for i in range(len(X1)):
  x_grid.append(X1[i][0])
  y_grid.append(X1[i][1])

if(d1 > d2):
  x_grid.append(x[0])
  y_grid.append(x[1])

plt.scatter(x_grid,y_grid)

x_grid = []
y_grid = []
for i in range(len(X2)):
  x_grid.append(X2[i][0])
  y_grid.append(X2[i][1])

if(d1 < d2):
  x_grid.append(x[0])
  y_grid.append(x[1])
plt.scatter(x_grid,y_grid)

if(d1 == d2):
  print('None')

plt.scatter(z1[0],z1[1])
plt.scatter(z2[0],z2[1])
plt.plot([z1[0],z2[0]],[z1[1],z2[1]])
plt.plot([0,0],[-3,10], color = 'black')
plt.plot([-6,10],[0,0], color = 'black')

print(z1)
print(z2)
grid1 = plt.grid()
plt.xlim((-6,10))
plt.ylim((-3,10))
if(z1[1] != z2[1]):
  plt.plot([3,4],[delta_d(3,z1,z2),delta_d(5,z1,z2)], color = 'red')
else:
  plt.plot([(z1[0] + z2[0])/2,(z1[0] + z2[0])/2], [-2,4])
plt.show()