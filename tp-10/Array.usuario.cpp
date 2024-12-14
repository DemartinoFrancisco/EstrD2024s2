#include <iostream>
using namespace std;
#include "Array.h";

int sumatoria(ArrayList xs) {
    int x = 0;
    for (int i=0; i<lengthAL(xs); i++) {
        x = x + get (i, xs);
    };
    return x;
};

void sucesores(ArrayList xs) {
    int x = 0;
    for (int i=0; i<lengthAL(xs); i++) {
        set(i, (get(i,xs)) +1 , xs);
    };
};

bool pertenece(int x, ArrayList xs) {
    for (int i = 1; i < lengthAL(xs); (get(i,xs)) != x ; i++) {
    };
    return (get(i,xs)) = x;
};

int apariciones(int x, ArrayList xs) {
    int n = 0;
    for (int i = 0; i < lengthAL(xs); i++) {
        if (get(i,xs) = x ) {
            n = n++;
        };
    };
    return n
};

ArrayList append(ArrayList xs, ArrayList ys) {
    ArrayListSt* arrayCombinado = newArrayListWith(lengthAL(xs) + lengthAL(ys));
    for (int i = 0; i < lengthAL(xs); i++) {
        add(get(i,xs),arrayCombinado);
    };
    for (int i = 0; i < lengthAL(ys); i++) {
        add(get(i,ys),xs);
    };
    return arrayCombinado;
};

int minimo(ArrayList xs) {
    int masChicoAlMomento = get(1,xs);
    for(int i=0; i<(lengthAL(xs)); i++) {
        masChicoAlMomento = menorEntre(masChicoAlMomento, get(i,xs));
    }
    return masChicoAlMomento;
};

int menorEntre(int n1, int n2) {
    if(n1<=n2) {
        return n1;
    } else {
        return n2;
    };
};