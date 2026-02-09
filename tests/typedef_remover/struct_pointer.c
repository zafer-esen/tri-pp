typedef struct Node {
    struct Node *next;
    int val;
} Node;

void main() {
    Node n;
    n.next = 0;
}
