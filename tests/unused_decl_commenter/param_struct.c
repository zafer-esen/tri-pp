struct S { int a; };
void f(struct S s) {}
void main() { struct S s; f(s); }
