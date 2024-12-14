#include <iostream>
using namespace std;
#include "Array.h";

struct ArrayListSt {
int cantidad; // cantidad de elementos
int* elementos; // array de elementos
int capacidad; // tamaño del array
};

ArrayList newArrayList() {
    ArrayListSt* al = new ArrayListSt;
    al -> cantidad  = 0;
    al -> elementos = new int[16];
    al -> capacidad = 16;
    return al;
};

ArrayList newArrayListWith(int capacidad) {
    ArrayListSt* al = new ArrayListSt;
    al -> cantidad  = 0;
    al -> elementos = new int[capacidad];
    al -> capacidad = capacidad;
    return al;
};

int lengthAL(ArrayList xs) {
    xs -> cantidad;
};

int get(int i, ArrayList xs) {
    if (i>0, i<(xs -> cantidad)) {
        return xs -> elementos[i];
    };
};

void set(int i, int x, ArrayList xs) {
    if (i>0, i<(xs -> cantidad)) {
        xs -> elementos[i] = x;
    };
};

void resize(int capacidad, ArrayList xs) {
    if (xs -> capacidad != capacidad) {
        int* nuevoArray = new int[capacidad];
        if (xs -> capacidad > capacidad) {
            for (int i=0; i <= xs -> cantidad; i++) {
                nuevoArray[i]   = xs -> elementos[i];
            };
            delete xs -> elementos;
            xs -> elementos = nuevoArray;
        } else {
            for (int i=0; i <= capacidad; i++) {
                nuevoArray[i]   = xs -> elementos[i];
            };
            delete xs -> elementos;
            xs -> elementos = nuevoArray;
        };
    };
    xs -> capacidad = capacidad;
};

void add(int x, ArrayList xs) {
    if (xs -> capacidad = xs -> cantidad) {
        int* nuevoArray = new int[xs -> capacidad * 2];
        for (int i=0; i <= xs -> cantidad; i++) {
            nuevoArray[i]   = xs -> elementos[i];
            nuevoArray[xs -> cantidad + 1] = x;
        };
        delete xs -> elementos;
        xs -> elementos = nuevoArray;
        xs -> capacidad = xs -> capacidad * 2;
        xs -> cantidad  = xs -> cantidad + 1;
    };
};

void remove(ArrayList xs) {
    xs -> cantidad = xs -> cantidad - 1;
};