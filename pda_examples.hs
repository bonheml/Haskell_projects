-- Accept the language {aibjck | i,j,k ≥ 0 and i=j or i=k}
ijk = (0,[3, 6],[((0, "", ""), (1, "x")),
               ((1, "a", ""), (1, "a")),
                ((1, "", ""), (2, "")),
                ((1, "", ""), (4, "")),
                ((2, "b", "a"), (2, "")),
                ((2, "", "x"), (3, "")),
                ((3, "c", ""), (3, "")),
                ((4, "b", ""), (4, "")),
                ((4, "", ""), (5, "")),
                ((5, "c", "a"), (5, "")),
                ((5, "", "x"), (6, ""))])

-- Accept the language {w ∈ {a,b,c}* |
-- the number of c's is equal to the number of a's plus the number of b's}
abc = (0,[2],[((0, "", ""), (1, "z")),
              ((1, "a", ""), (1, "x")),
              ((1, "b", ""), (1, "x")),
              ((1, "c", ""), (1, "c")),
              ((1, "a", "c"), (1, "")),
              ((1, "b", "c"), (1, "")),
              ((1, "c", "x"), (1, "")),
              ((1, "", "z"), (2, ""))])