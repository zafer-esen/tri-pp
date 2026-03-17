class C {
    int x;
};

int main() {
    C c1;
    C *c2 = &c1;
    C &c3 = c1;
    C **c4 = &c2;

    C c5, *c6;

    return 0;
}
