/* an array parameter of unknown bound. */
int sum(int a[], int n) {
  int s = 0;
  int i;
  for (i = 0; i < n; i++) s += a[i];
  return s;
}
int main() {
  int xs[3] = {1, 2, 3};
  return sum(xs, 3);
}
