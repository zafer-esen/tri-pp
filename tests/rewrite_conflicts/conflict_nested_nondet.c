/* Under --determinize both calls are rewritten, and the replacement of
   the outer call overlaps the replacement of its nested argument: the
   composed text depends on transformer order. tri-pp must refuse
   (exit code 4) instead of producing silent garbage */
extern int nondet_a(void);
extern int nondet_b(int);

int main() {
  int x = nondet_b(nondet_a());
  return x;
}
