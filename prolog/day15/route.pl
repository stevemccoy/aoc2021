% Route finding example.

conn(start, e, 2).
conn(e, f, 5).
conn(f, g, 2).
conn(g, t, 2).
conn(t, d, 3).
conn(d, c, 3).
conn(c, b, 2).
conn(b, a, 2).
conn(a, start, 2).

s(A, B, G) :- conn(A, B, G).
s(A, B, G) :- conn(B, A, G).

h(start, 15).
h(e, 7).
h(a, 5).
h(b, 4).
h(f, 4).
h(g, 2).
h(c, 4).
h(d, 3).
h(t, 0).

goal(t).
