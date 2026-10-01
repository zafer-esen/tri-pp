#define Pre 1
#define Here 2
#define LoopEntry 3
#define OFS 4
int f(int x) {
  //@ assert \at(x, Pre) + OFS == \at(x, Here) + OFS;
  /*@ loop invariant \at(x, LoopEntry) <= x + OFS; */
  while (x < 10) { x++; }
  return x;
}
