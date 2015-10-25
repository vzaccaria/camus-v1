```
i........ s
.c.....c.     oo
.........
.v.v.q.q.  g g
.r.....r.
.k.....k.  a        a
........i            s
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


| src  | dst  | style       | curve | label               | through  | pattern |
| ---- | ---- | -----       | ----- | -----               | -------- | ------- |
| q    | v    | -latex      |       | \tiny data          |          |         |
| c    | v    | -latex      |       | \tiny view template |          |         |
| c    | q    | -latex      |       | \tiny stored query  |          |         |
| g    | q    | latex-latex |       | \tiny graphql       |          |         |
