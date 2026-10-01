#define LIMIT 240
typedef int myint;
int f(int x) {
  if (x)
    //@ assert x != LIMIT;
    x = 1;
  for (int i = 0; i < 3; i++)
    /*@ assert i < LIMIT; */
  {
    x += i;
  }
  while (x > LIMIT)
    //@ assert x > LIMIT;
    x--;
  //@ assert x < LIMIT;
  myint y = x;
  /*@ assert y < LIMIT; */ myint z = y;
  return z;
}
//@ predicate P(int v) = v < LIMIT;
myint g(myint a);
