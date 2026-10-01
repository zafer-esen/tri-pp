#define x x
#define y (y + x)
#define F(a) F(a) + 1
int f(int x) {
  //@ assert y > F(x);
  return x;
}
