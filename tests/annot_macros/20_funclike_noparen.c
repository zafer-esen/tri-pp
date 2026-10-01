#define GOOD(x) ((x) >= 240)
int f(int x) {
  //@ assert GOOD;
  x = GOOD(x);
  /*@ assert GOOD */ (x);
  return x;
}
