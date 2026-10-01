#define LIMIT 240
#define BOUND (LIMIT + 1)
#define INRANGE(v) ((v) >= 0 && (v) < BOUND)
void f(int x) {
  //@ assert INRANGE(x) && INRANGE(LIMIT);
  x = 0;
}
