// Annotations whose macros cannot be expanded are kept verbatim and listed
// with their position, the reason and their first line (YAML-escaped).
#define CLOSE "*/"
#define ID(v) v
#define POS(v) ((v) > 0)
typedef int myint;
int main(void) {
  myint x = 1;
  char *s = "a";
  /*@ assert s != CLOSE && '#' != ':' && "\"" != s;
      assert x > 0; */
  //@ assert POS(x);
  //@ assert x > 0; ID(\)
  return x;
}
