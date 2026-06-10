namespace university {
    namespace department {
        class Teacher {
        public:
            struct Details {
                int age;
                char* name;
            };
            void setDetails(Details d) { info = d; }
            Details getDetails() const { return info; }
        private:
            Details info;
        };
    }
}

int main() {
    university::department::Teacher t;
    university::department::Teacher::Details d;
    t.setDetails(d);
    return 0;
}
