template <typename T>
class Box {
public:
    Box(T val) : value(val) {}
    T getValue() const { return value; }
private:
    T value;
};

typedef Box<int> IntBox;
using DoubleBox = Box<double>;

int main() {
    IntBox ib(10);
    DoubleBox db(20.5);
    Box<char> cb('a');
    
    int v = ib.getValue();
    double d = db.getValue();
    
    return 0;
}
