
% switch to list of characters rather than tokens

:- set_prolog_flag(double_quotes, chars).

s --> np, " ", vp.

np --> det, " ", n.

vp --> v, " ", np.
vp --> v.

det --> "the".
det --> "a".

n --> "woman".
n --> "man".

v --> "shoots".



