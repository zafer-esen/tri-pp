/* throw and try/catch are reported, TriCera transforms exceptions away. */
int risky(int v) {
  if (v < 0)
    throw 42;
  return v + 1;
}

int main() {
  try {
    return risky(1);
  } catch (int e) {
    return e;
  }
}
