#include "tree.h";
#include <iostream>
using namespace std;

int sumarT(Tree t) {
//Dado un árbol binario de enteros devuelve la suma entre sus elementos.
    if (isEmptyT(t)) {
        return 0;
    } else {
        return(rootT(t) + (sumarT(left(t))) + (sumarT(right(t))));
    };
};

int sizeT(Tree t) {
//Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size en inglés).
    if(isEmptyT(t)) {
        return 0;
    } else {
        return 1 + sizeT(t) + sizeT(t);
    };
};

bool perteneceT(int e, Tree t) {
//Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el árbol.
    if(isEmptyT(t)) {
        return false;
    } else {
        rootT(t) == e || perteneceT(e, left(t)) || perteneceT(e, right(t));
    };
};

int aparicionesT(int e, Tree t) {
//Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son iguales a e.
};

int heightT(Tree t) {
//Dado un árbol devuelve su altura.
};

//ArrayList toList(Tree t) {
//Dado un árbol devuelve una lista con todos sus elementos.
//};

//ArrayList leaves(Tree t) {
//Dado un árbol devuelve los elementos que se encuentran en sus hojas.
//};

//ArrayList levelN(int n, Tree t) {
//Dados un número n y un árbol devuelve una lista con los nodos de nivel n
//};