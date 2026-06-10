/* the program declares its own arrays (one of unknown bound) and also
   gets determinizer inputs: the facts keep the two apart. */
extern int nondet_int(void);
int own_unbounded[];
int main() {
  int own_sized[2] = {0, 1};
  int n = nondet_int();
  int acc = own_sized[0] + own_unbounded[0];
  while (n > 0) {
    acc += nondet_int();
    n--;
  }
  return acc;
}
