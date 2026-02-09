void recurse(int x) {
    if (x > 0) recurse(x-1);
}

void main() {
    recurse(10);
}
