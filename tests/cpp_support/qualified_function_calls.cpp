class A {
  public:
    int x;
    void set(int val) {
      x = val;
    }
    int get() {
      return x;
    }

    virtual int num() {
      return 1;
    }
};
class B : public A {
  public:
    int y;
    int num() override {
      return 2;
    }
};


class C {
  public:
    int x;
    C(int val) {
      x = val;
    }

    ~C() {}
};

class Unused {
};

int main() {
  A a;
  a.set(1);
  a.get();

  B b;
  A *a_ptr = &b;

  a_ptr->num(); // Should not be qualified as it is dynamically dispatched
  b.num(); // Should not be qualified, still goes via vtable

  C c(10);

  c.C::~C();

  return 0;
}
