#define LIMIT 240 //@ assert LIMIT > 0;
#define LIM2 1 /*@ assert LIMIT > 0; */ + 1
#if LIMIT //@ assert LIMIT;
#endif //@ assert LIMIT;
#ifdef LIMIT //@ y LIMIT
int b;
#endif /*@ w
          LIMIT */
int f(int x) { return x + LIM2; }
