typedef struct {
    int x;
} Inner;

typedef struct {
    Inner i;
    int y;
} Outer;

void main() {
    Outer o;
    o.i.x = 1;
}
