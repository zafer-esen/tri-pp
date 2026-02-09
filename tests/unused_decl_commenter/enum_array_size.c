typedef enum {
    FIRST,
    SECOND,
    MyEnum_COUNT
} MyEnum;

int array[MyEnum_COUNT];

void main(void) {
    if (array[FIRST] == 0) {}
}
