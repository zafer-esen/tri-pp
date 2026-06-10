/* All default-pipeline rewrites on this input are geometry-preserving
   (blanked typedefs and unused decls, canonised types, char literals,
   loop guard rewriting): the output must contain zero linemarkers. */
extern int nondet_int(void);

typedef int myint;
typedef unsigned long ulong_t;

struct unused_struct { int x; };
enum unused_enum { U_A, U_B };

void unused_fun(void) { }

int main() {
  myint probe_a = 'a';
  ulong_t probe_b = 0;
  while (nondet_int()) {
    probe_b += probe_a;
  }
  int probe_c = (int) probe_b + '\n';
  return probe_c;
}
