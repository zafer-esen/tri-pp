#define LIMIT 240
void f(int x) {
#if 0
  //@ assert x < LIMIT;
#endif
  //@ assert x <= LIMIT;
  x = 0;
}
