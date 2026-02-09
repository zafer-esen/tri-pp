struct S { int x; };
void f(struct S* p) { p->x = 1; }
void main() { struct S s; f(&s); }
