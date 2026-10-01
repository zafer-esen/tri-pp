#define LIMIT 240
typedef struct { int a; } T;
int f(T t) {
  int r = (int) /*@ assert LIMIT; */ t.a;
  T u = /*@ ghost int k = LIMIT; */ t;
  switch (r) { case 1: /*@ assert r == LIMIT; */ break; default: ; }
  return r + u.a + sizeof(/*@ assert LIMIT; */ T);
}
