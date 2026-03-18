class C {
  public:
    int val;

    void set(int value) {
      val = value;
    }

    void shadowed_set(int val) {
      val = val;
    }

    void qualified_set(int val) {
      this->val = val;
    }
};

int main() {
  C c;
  c.set(5);
  return 0;
}
