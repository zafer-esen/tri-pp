#define GOOD(x) ((x) >= 240)
void entry(void) { unsigned char status = 255; //@ assert GOOD(status);
}
