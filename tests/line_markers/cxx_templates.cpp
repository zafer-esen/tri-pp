// The class template is monomorphised and its instantiation injected,
// which shifts the lines after it. The markers must re-sync so the probed
// variables in the non-template code below still map to their original
// lines. (Names inside the template body are reprinted into the
// instantiation and map only approximately, by contract, so none are put
// there.)
template <typename T>
class Box {
public:
  Box(T v) : value(v) {}
  T get() const { return value; }
private:
  T value;
};

int helper(int probe_arg) {
  return probe_arg + 1;
}

int main() {
  Box<int> b(7);
  int probe_a = b.get();
  int probe_b = helper(probe_a);
  return probe_a + probe_b;
}
