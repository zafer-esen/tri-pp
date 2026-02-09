extern int nondet_int();
void main() {
  while(nondet_int()) {
    int x = 1;
  }
}
