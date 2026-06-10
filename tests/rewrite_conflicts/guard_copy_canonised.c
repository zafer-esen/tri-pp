/* NondetLoopGuardRewriter copies the guard into the counter
   initialisation and blanks the original; a later round canonises the
   sizeof inside the copy like any other code. */
extern int nondet(int);

int main() {
  int *p;
  int n = 0;
  while (nondet(sizeof *p)) {
    n++;
  }
  return n;
}
