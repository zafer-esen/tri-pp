#define ALL(...) (__VA_ARGS__)
#define FIRST(a, ...) (a)
void f(int x, int y) {
  //@ assert ALL(x > 0, y > 0) && FIRST(x, y, 3) > 0;
  x = 0;
}
