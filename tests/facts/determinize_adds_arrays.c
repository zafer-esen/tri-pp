/* --determinize replaces the nondet calls with an input variable and an
   unbounded input array; these are reported as inputs, separately from
   the program's own (here: none) array declarations. */
extern int nondet_int(void);
int main() {
  int n = nondet_int();
  int acc = 0;
  while (n > 0) {
    acc += nondet_int();
    n--;
  }
  return acc;
}
