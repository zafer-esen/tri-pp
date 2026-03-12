template <typename T>
class Box {
public:
    Box(T val) : value(val) {}
    T getValue() const { return value; }
    void setValue(T val) { value = val; }
private:
    T value;
};

class Base {
public:
    virtual void show() {}
};

class Derived : public Base {
public:
    void show() override {}
    static int getKind() { return 1; }
};

int main() {
    Box<int> intBox(10);
    Box<double> doubleBox(20.5);
    Derived d;
    return 0;
}
