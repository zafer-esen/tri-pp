# 100 "original.c"
typedef int myint;

int compute(int probe_v) {
  return probe_v + 1;
}

int main() {
# 200 "original.c"
  myint probe_a = compute(1);
#line 300 "original.c"
  int probe_b = compute(probe_a);
  return probe_b;
}
