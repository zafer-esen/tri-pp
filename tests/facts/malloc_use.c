/* malloc and free referenced in live code. */
extern void *malloc(unsigned long);
extern void free(void *);
int main() {
  int *p = malloc(sizeof *p);
  *p = 4;
  int v = *p;
  free(p);
  return v;
}
