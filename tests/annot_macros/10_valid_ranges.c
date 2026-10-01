#define n 10
#define LIMIT 240
#define N 5
#define size 8
#define VALID(p,k) \valid(p+(0..k-1))
#define VALID2(p,k) \valid(p+(0 .. k-1))
/*@ requires \valid(a+(0..n-1));
    requires \valid(a+(0..LIMIT)) && \valid(a+(1..N-1));
    requires \valid(a+(0..size-1));
    requires VALID(a, 10) && VALID2(a, n);
    requires \valid(a+(0..9)) && \valid(a+(0 .. 9)) && \valid(a+(1.5..2));
    assigns a[0..n-1], a[N..LIMIT];
 */
int f(int *a) { return a[0]; }
