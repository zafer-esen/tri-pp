/* An expansion must not change where the annotation comment ends. */
#define CLOSE "*/"
#define TIMES x*
#define HALF(v) v/2
#define ID(v) v
int f(int x, char *s) {
  /*@ assert s != CLOSE; */
  //@ assert s != CLOSE;
  /*@ assert TIMES/2 > 0; */
  /*@ assert TIMES HALF(x) > 0; */
  //@ assert TIMES/2 > 0;
  //@ assert x > 0; ID(\)
  return x;
}
