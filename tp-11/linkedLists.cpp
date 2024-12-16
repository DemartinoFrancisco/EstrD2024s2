#include "linkedLists.h";
#include <iostream>
using namespace std;

struct NodoL {
int elem; // valor del nodo
NodoL* siguiente; // puntero al siguiente nodo
};
struct LinkedListSt {
// INV.REP.: cantidad indica la cantidad de nodos que se pueden recorrer
// desde primero por siguiente hasta alcanzar a NULL
int cantidad; // cantidad de elementos
NodoL* primero; // puntero al primer nodo
};
struct IteratorSt {
NodoL* current;
};

LinkedList nil() {
//Crea una lista vacía.
    LinkedListSt* listaEnlazada = new LinkedListSt;
    listaEnlazada -> primero    = NULL;
    listaEnlazada -> cantidad   = 0;
    return listaEnlazada;
};

bool isEmpty(LinkedList xs) {
//Indica si la lista está vacía.
    return xs -> primero == NULL;
};

int head(LinkedList xs) {
//Devuelve el primer elemento.
    return (xs -> primero) -> elem;
};

void Cons(int x, LinkedList xs) {
//Agrega un elemento al principio de la lista.
    NodoL* nuevoNodo = new NodoL;
    nuevoNodo -> elem = x;
    nuevoNodo -> siguiente = xs -> primero;
    xs -> primero = nuevoNodo;
    xs -> cantidad = xs -> cantidad + 1;
};

void Tail(LinkedList xs) {
//Quita el primer elemento.
    if (xs -> cantidad > 0) {
        NodoL* temp = xs -> primero;
        xs -> primero = (xs -> primero) -> siguiente;
        delete temp;
        xs -> cantidad = xs -> cantidad - 1;
    };
};

int length(LinkedList xs) {
//Devuelve la cantidad de elementos.
    return xs -> cantidad;
};

void Snoc(int x, LinkedList xs) {
//Agrega un elemento al final de la lista.
    NodoL* nuevoNodo = new NodoL;
    nuevoNodo -> elem = x;
    nuevoNodo -> siguiente = NULL;
    if (xs ->primero == NULL) {
        xs ->primero = nuevoNodo;
    } else {
        ListIterator iterador = getIterator(xs);
        while(! (elSiguienteEsElUltimo(iterador))) {
            Next(iterador);
        };
        delete iterador -> current -> siguiente;
        (iterador -> current) -> siguiente == nuevoNodo;
        DisposeIterator(iterador);
    };
    xs -> cantidad = xs ->cantidad + 1;
};

bool elSiguienteEsElUltimo(ListIterator ixs) {
    return ((ixs -> current) -> siguiente) -> siguiente == NULL;
};

ListIterator getIterator(LinkedList xs) {
//Apunta el recorrido al primer elemento.
    IteratorSt* iterador = new IteratorSt;
    iterador -> current = xs -> primero;
    return iterador;
};

int current(ListIterator ixs) {
//Devuelve el elemento actual en el recorrido.
    return (ixs -> current) -> elem;
};

void SetCurrent(int x, ListIterator ixs) {
//Reemplaza el elemento actual por otro elemento.
    (ixs -> current) -> elem = x;
};

void Next(ListIterator ixs) {
//Pasa al siguiente elemento.
    ixs -> current = (ixs -> current) -> siguiente;
};

bool atEnd(ListIterator ixs) {
//Indica si el recorrido ha terminado.
    return (ixs -> current) -> siguiente == NULL; 
};

void DisposeIterator(ListIterator ixs) {
//Libera la memoria ocupada por el iterador.
    delete ixs;
};

void DestroyL(LinkedList xs) {
//Libera la memoria ocupada por la lista.
    while (! isEmpty(xs) ) {
        Tail(xs);
    }
    delete xs;
};

void Append(LinkedList xs, LinkedList ys) {
//Agrega todos los elementos de la segunda lista al final de los de la primera. La segunda lista se destruye.
    ListIterator iterador = getIterator(xs);
    while (! atEnd(iterador) ) {
        Next(iterador);
    };
    iterador -> current -> siguiente = ys -> primero;
    delete ys;
    DisposeIterator(iterador);
};

