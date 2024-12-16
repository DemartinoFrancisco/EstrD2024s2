#include "tree.h";
#include <iostream>
using namespace std;

struct NodeT {
int elem;
NodeT* left;
NodeT* right;
};

Tree emptyT() {
    return NULL;
};

Tree nodeT(int elem, Tree left, Tree right) {
    NodeT* arbol = new NodeT;
    arbol -> elem = elem;
    arbol -> left = left;
    arbol -> right = right;
    return arbol;
};

bool isEmptyT(Tree t) {
    return t == NULL;
};

int rootT(Tree t) {
    return t -> elem;
};

Tree left(Tree t) {
    return t -> left;
};

Tree right(Tree t) {
    return t -> right;
};