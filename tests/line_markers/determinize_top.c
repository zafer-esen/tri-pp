/* --determinize inserts global input declarations at the top of the
   file; every original line must still map to itself. */
extern int nondet_int(void);

int main() {
  int probe_x = nondet_int();
  int probe_acc = 0;
  while (probe_x > 0) {
    probe_acc += nondet_int();
    probe_x--;
  }
  return probe_acc;
}
