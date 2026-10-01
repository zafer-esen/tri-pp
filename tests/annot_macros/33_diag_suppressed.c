/* With -pedantic, expanding VA() with no variadic argument would warn.
   Diagnostics during expansion are suppressed: the annotation is still
   expanded and nothing is printed. */
#define VA(a, ...) (a)
int f(int x) {
  //@ assert VA() == 0 && LIM > 0;
  return x;
}
