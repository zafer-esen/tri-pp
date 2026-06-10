/* --make-calls-unique clones `helper` for its second call site; the
   cloned body must map back to helper's original lines. */
extern int nondet_int(void);

int helper(int a) {
  int probe_helper_body = nondet_int();
  return a + probe_helper_body;
}

int main() {
  int x = helper(1);
  int y = helper(2);
  int probe_main = x + y;
  return probe_main;
}
