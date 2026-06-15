class C {
  public:
    int x;
    C(int val) {
      x = val;
    }

    ~C() { }
};


int main() {
  C obj(1);
  obj.~C();
  return 0;
}
