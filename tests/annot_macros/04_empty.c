#define NOTHING
#define EMPTYF(a)
void f(int x) {
  //@ assert NOTHING x > 0 EMPTYF(zz);
  x = 0;
}
