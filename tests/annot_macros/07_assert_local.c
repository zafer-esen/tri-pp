#define assert(e) ((void)0)
#define requires 1
#define loop 2
#define invariant 3
void f(int x) {
  //@ assert (x > 0);
  /*@ loop invariant x >= 0; */
  while (x > 0) x--;
}
