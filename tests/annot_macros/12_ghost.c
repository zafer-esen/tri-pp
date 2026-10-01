#define LIMIT 240
void f(void) {
  /*@ ghost int g = LIMIT; */
  int x = 0;
  /*@ ghost g = g + LIMIT; */
}
