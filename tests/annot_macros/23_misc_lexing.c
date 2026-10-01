#define LIMIT 240
#define ID(x) x
int f(int x) {
  //@ assert x < \
     LIMIT;
  //@ assert x < LIMIT; //@ assert x > LIMIT;
  /*@ assert x < LIMIT; */ /*@ assert ID(x) < ID(
        LIMIT); */
  /*@ assert x < LIMIT; // trailing LIMIT comment
      assert x > ID(1); */
  //@ assert c == '*' && s == "LIMIT /* x */" && 0..LIMIT;
  return x;
}
