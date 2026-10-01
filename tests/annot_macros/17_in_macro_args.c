#define LIMIT 240
#define FOO(a, b) ((a) + (b))
int f(int x) {
  int r = FOO(x, //@ assert x < LIMIT;
     LIMIT);
  int s = FOO /*@ assert x < LIMIT; */ (1, 2);
  return r + s;
}
