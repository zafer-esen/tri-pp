/* with --no-decl-slice nothing is sliced away; typedefs are still
   removed (that is not slicing), so their uses must still be canonised. */
typedef int unused_t;
struct unused_s { int f; };
void unused_fun(void) { }
int main() { return 0; }
