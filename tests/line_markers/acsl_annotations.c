/* ACSL annotations must pass through byte-identical, 
   no marker may be placed inside one, and
   probes inside them must keep mapping to their original lines. The clone
   of helper is inserted right before the contract of `annotated`. */
extern int nondet_int(void);

int helper(int a) {
  int probe_h = nondet_int();
  return a + probe_h;
}

/*@ requires probe_v > 0;
  @ ensures \result == probe_v + 1;
  @*/
int annotated(int probe_v) {
  return probe_v + 1;
}

int main() {
  int x = helper(1);
  int y = helper(2);
  int probe_m = annotated(x + y);
  return probe_m;
}
