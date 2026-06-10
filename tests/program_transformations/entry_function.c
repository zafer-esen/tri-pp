/* with -m foo, slicing is relative to foo: helper stays, main and the
   unused struct are removed. */
int helper(int x) { return x + 1; }
struct unused_s { int f; };
int foo(void) { return helper(41); }
int main() { return 0; }
