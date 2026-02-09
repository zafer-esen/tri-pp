extern int nondet_int();

int foo () {
  return 42;
}

int bar() {
  return foo();
}

void main()
{
  int x = nondet_int();
  while(nondet_int()) {
    x = nondet_int();
  }
  assert(x == nondet_int());

  int y1 = foo();
  int z1 = foo();

  int y2 = bar();
  int z2 = bar();

  assert(y1 == y2);
}
