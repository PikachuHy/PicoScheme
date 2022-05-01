# Machine Model

## A Simple Target Machine Model

### Instructions

- Load operations
  LD dst, addr
  LD R1, a(R2)
  => R1=contents(a + contents(R2))
  LD R1, 100(R2)
  => R1=contents(100+contents(R2))
  LD R1, *100(R2)
  => R1=contents(contents(100+contents(R2)))
  LD R1, #100
- Store operations
  ST x, r
  => x=r
- Computation operations
  OP dst, src_1, src_2
  ADD
  SUB
  MUL

- Unconditional jumps
  BR L
- Conditional jumps
  BLTZ r, L

### Costs

1 LD R0, R1
2 LD R0, M
2 LD R1, *100(R2)
