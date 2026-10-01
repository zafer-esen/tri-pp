#define STR(x) #x
#define CAT(a,b) a##b
void f(int xy) {
  //@ assert CAT(x,y) > 0 && STR(hello) != 0;
  xy = 0;
}
