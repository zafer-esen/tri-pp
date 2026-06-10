class Outermost {
  public:
    class Inner {
      public:
        class Innermost {
        };
    };
};


int main() {
  Outermost x;
  Outermost::Inner y;
  Outermost::Inner::Innermost z;
  return 0;
}
