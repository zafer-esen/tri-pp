extern int nondet_int();
extern int nondet_int_copy_1();
extern int nondet_int_copy_2();
extern int nondet_int_copy_3();

int foo () {
  return 42;
}

int bar() {
  return foo();
}

int bar_copy_1() {
  return foo();
}

void main()
{
  int x = nondet_int();
  while(nondet_int_copy_1()) {
    x = nondet_int_copy_2();
  }
  assert(x == nondet_int_copy_3());

  int y1 = foo();
  int z1 = foo();

  int y2 = bar();
  int z2 = bar_copy_1();

  assert(y1 == y2);
}
