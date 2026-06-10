/* a sized array declaration: usesArrays without usesUnboundedArrays. */
int main() {
  int xs[3] = {1, 2, 3};
  return xs[0] + xs[1] + xs[2];
}
