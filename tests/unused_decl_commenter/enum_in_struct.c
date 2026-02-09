enum E { A };
struct S { enum E e; };
void main() { struct S s; s.e = A; }
