#define DIAG_OFF _Pragma("GCC diagnostic ignored \"-Wunused-value\"")
#define PACK _Pragma("pack(1)")
#define WRAP(e) (e && DIAG_OFF 1)
int f(int x) {
  //@ assert __COUNTER__ == 0 && DIAG_OFF x > 0;
  x;
  //@ assert PACK x > 0;
  //@ assert __COUNTER__ >= 0;
  //@ assert WRAP(x > 0);
  return __COUNTER__;
}
struct S { char c; int i; };
int sz = sizeof(struct S);
