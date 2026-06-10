template <typename T1, typename T2>
class Pair {
public:
    T1 first;
    T2 second;
    Pair(T1 a, T2 b) : first(a), second(b) {}
};

template <typename T>
T add(T a, T b) {
    return a + b;
}

int main() {
    Pair<int, char> p(1, 'a');
    int sum = add<int>(5, 10);
    double dsum = add(1.5, 2.5);
    return 0;
}
