
% just as good, but call the remainder R; can make R=[] if need to ensure no remainder

s(X,R) :- np(X,Y), vp(Y,R). 

np(X,R) :- det(X,Y), n(Y,R). 

vp(X,R) :- v(X,Y), np(Y,R). 

vp(X,R) :- v(X,R). 

% i.e., starts with a particular token
det([the|W],W). 
det([a|W],W). 

n([woman|W],W). 
n([man|W],W). 

v([shoots|W],W).

