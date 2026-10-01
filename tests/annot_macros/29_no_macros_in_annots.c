/* Macros used only in code; the annotations use none (one names a
   function-like macro without arguments). With --expand-annot-macros the
   output must be byte-identical to the output without it. */
#define LIMIT 240
#define GOOD(x) ((x) >= LIMIT)
typedef int myint;
extern int nondet(void);
/*@ requires 0 <= n && n <= 100;
  @ requires \valid(a+(0..n-1));
  @ ensures \result == \old(n) + 1;
  @ assigns a[0..9];
  @*/
int f(int *a, int n) {
  myint k = LIMIT;
  //@ assert k == 240 && GOOD;
  /*@ loop invariant 0 <= k; */
  while (k > 0 && GOOD(k)) { k--; }
  //@ ghost int g = 0;
  return n + 1;
}
int unused(int u) { return u; }
int main() { int a[10]; return f(a, nondet()); }
