class MyException {};

void f() {
  throw MyException();
}

void g(int x) {
  if (x > 0) throw x;
  if (x < 0) throw "error";
  throw 1.0 + 2.0;
}

struct S {
    int a;
};

void h() {
    S s = {1};
    throw s;
}

void i() {
    throw 'a';
}

int main() {
  try {
    f();
  } catch (MyException& e) {
  }
  try {
    g(1);
  } catch (...) {}
  return 0;
}
