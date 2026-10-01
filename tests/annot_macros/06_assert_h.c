#include <assert.h>
void f(int x) {
  //@ assert (x > 0);
  assert(x > 0);
}
