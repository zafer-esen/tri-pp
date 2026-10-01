#define N 10
#define INV(i) (0 <= (i) && (i) <= N)
int f(void) {
  int s = 0;
  int i;
  /*@ loop invariant INV(i);
      loop assigns i, s;
      loop variant N - i; */
  for (i = 0; i < N; i++) { s += i; }
  return s;
}
