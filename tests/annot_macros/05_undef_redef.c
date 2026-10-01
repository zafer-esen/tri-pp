#define LIMIT 10
void f(int x) {
  //@ assert x < LIMIT;
#undef LIMIT
#define LIMIT 20
  //@ assert x < LIMIT;
#undef LIMIT
  //@ assert x < LIMIT;
  x = 0;
}
