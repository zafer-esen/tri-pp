class MyException {};

void f() {
  // Existing elaborated cast
  throw (class MyException) MyException();
}

void g() {
  // Existing unelaborated cast
  throw (MyException) MyException();
}

struct S { int a; };

void h() {
  // Existing struct cast
  throw (struct S) S{1};
}

int main() {
  try { f(); } catch (...) {}
  try { g(); } catch (...) {}
  try { h(); } catch (...) {}
  return 0;
}
