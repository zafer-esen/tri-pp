#define NEG -1
#define PLUS +
#define EMPTY
#define ID(a) a
#define LIMIT 240
#define TWO(a, b) ((a) + (b))
int f(int x) {
  //@ assert x == -NEG && x == -EMPTY-1 && x PLUS+1 > 0 && x+LIMIT > 0;
  //@ assert ID(x \
     ) > 0 && LIMIT > 0;
  /*@ requires TWO(x,
    @            LIMIT) > 0;
    @ ensures \result >= ID(
    @   LIMIT);
    @*/
  return x;
}
