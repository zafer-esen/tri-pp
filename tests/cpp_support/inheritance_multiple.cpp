class Drawable {
public:
    virtual void draw() = 0;
};

class Clickable {
public:
    virtual void onClick() {}
};

class Button : public Drawable, public Clickable {
public:
    void draw() override {}
    void onClick() override {}
    static int getClickCount() { return count; }
private:
    static int count;
};

int Button::count = 0;

int main() {
    Button b;
    b.draw();
    b.onClick();
    return 0;
}
