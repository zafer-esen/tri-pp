struct A { int x; };
struct B { struct A a; };
void main() { struct B b; b.a.x = 1; }
