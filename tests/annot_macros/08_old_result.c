#define old 42
#define result 7
/*@ ensures \result == \old(x) + old; */
int f(int x) { return x + 42; }
