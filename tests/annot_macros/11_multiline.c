#define LIMIT 240
#define POS(x) ((x) > 0)
/*@ requires POS(n);
  @ requires n < LIMIT;
  @ ensures \result == n +
  @   LIMIT;
  @ assigns \nothing; */
int f(int n) { return n + LIMIT; }
/*@ requires POS(
  @   n) && n < LIMIT;
  @ ensures \result == n + LIMIT;
  @ assigns \nothing; */
int g(int n) { return n + LIMIT; }
