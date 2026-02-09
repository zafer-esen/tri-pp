struct S { int x; };
void f(int* p) { *p = 1; }
void main() { int s; f(&s); }
