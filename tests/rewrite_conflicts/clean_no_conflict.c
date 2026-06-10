/* Same shape without the nested rewrite: no conflict, output written. */
extern int nondet(void);

int main() {
  int n = 0;
  while (nondet()) {
    n++;
  }
  return n;
}
