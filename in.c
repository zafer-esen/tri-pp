#include <assert.h>

typedef struct{
    int a;
}MyStruct;

typedef struct S{
    int a;
}MyStruct2;

MyStruct s;
MyStruct2 s2;

int main () {
    assert(s.a == 0);
    assert(s2.a == 0);
    for (int i = 0; i < 42; ++i) {
      assert(i >= s2.a);
    }
    return 0;
}
