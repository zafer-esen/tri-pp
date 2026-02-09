struct S { int x; };
typedef struct S T1, *T2;

void main() {
    T1 a;
    T2 b = &a;
}
