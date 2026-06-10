/* the only malloc sits in an unused function that tri-pp removes;
   the produced program references no allocation function. */
extern void *malloc(unsigned long);
void unused_heap_user(void) {
  int *p = malloc(4);
}
int main() {
  return 0;
}
