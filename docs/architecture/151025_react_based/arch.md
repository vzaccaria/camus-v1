```
i........ s
.c.....c.     o o
.........
.v.v.q.q.     g g
.r.....r.
.k.....k.   x  y  z
........i           s
```



| node  | label               | type                   |
| ----- | -----               | ---                    |
| v     | view renderer       | rectangle              |
| q     | query exec          | rectangle              |
| c     | configuration       | rectangle,fill=gray!10 |
| k     | app core            | rectangle              |
| i     |                     | rectangle              |
| r     | reactjs             | rectangle              |
| s     |                     | rectangle              |
| g     | graphql endpoint    | rectangle              |
| a     | aggregator          | rectangle              |
| o     | root context config | rectangle              |
| x     | s1                  | circle                 |
| y     | s2                  | circle                 |
| z     | s3                  | circle                 |


| src  | dst  | style       | curve | label               | through  | pattern |
| ---- | ---- | -----       | ----- | -----               | -------- | ------- |
| q    | v    | -latex      |       | \tiny data          |          |         |
| c    | v    | -latex      |       | \tiny view template |          |         |
| c    | q    | -latex      |       | \tiny stored query  |          |         |
| g    | q    | latex-latex |       | \tiny graphql       |          |         |
| o    | c    | -latex      |       | \tiny config time   |          |         |
| o    | g    | -latex      |       | \tiny access rules  |          |         |
| x    | g    | -latex      |       |                     |          |         |
| y    | g    | -latex      |       |                     |          |         |
| z    | g    | -latex      |       |                     |          |         |
