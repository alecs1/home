#include <stdio.h>


template <typename T>
struct Node {
    T val;
    Node* next;
};


template <typename T>
Node<T>* appendNode(Node<T>* inListNode, Node<T>* n) {
    while (inListNode->next != NULL)
        inListNode = inListNode->next;
    inListNode->next = n;
    n->next = NULL;
    return inListNode;
}

template <typename T>
Node<T>* reverseList(Node<T>* start) {
    Node<T>* crt = start->next;
    start->next = NULL;

    while (crt != NULL) {
        Node<T>* next = crt->next;
        crt->next = start;
        start = crt;
        crt = next;
    }

    return start;
}

template <typename T>
int printfList(Node<T>* node) {
    while (node != NULL) {
        printf("%d ", node->val);
        node = node->next;
    }
    printf("\n");
    return 0;
}

template <typename T>
int printfListFullNodes(Node<T>* node) {

    while (node != NULL) {
        printf("%d %p\t", node->val, node->next);
        node = node->next;
    }
    printf("\n");
    return 0;
}

int main(int argc, char* argv[]) {
    Node<int> start;
    start.val = 15;
    start.next = NULL;
    
    for(int i = 0; i < 20; i++) {
        Node<int> *node = new Node<int>;
        node->val = i;
        appendNode(&start, node);
    }

    printfList(&start);
    printfListFullNodes(&start);

    Node<int> *newStart = reverseList(&start);
    printfList(newStart);
    printfListFullNodes(newStart);


}
