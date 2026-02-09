struct S { int a; };
struct S f() { struct S s; return s; }
void main() { f(); }
