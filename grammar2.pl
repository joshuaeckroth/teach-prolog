
s(X,Z) :- np(X,Y), vp(Y,Z). 

np(X,Z) :- det(X,Y), n(Y,Z). 

vp(X,Z) :- v(X,Y), np(Y,Z). 

vp(X,Z) :- v(X,Z). 

det([the|W],W). 
det([a|W],W). 

n([woman|W],W). 
n([man|W],W). 

v([shoots|W],W).

