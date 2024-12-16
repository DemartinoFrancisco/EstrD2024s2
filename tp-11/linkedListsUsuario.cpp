#include "linkedLists.h";
#include <iostream>
using namespace std;

int sumatoria(LinkedList xs) {
//Devuelve la suma de todos los elementos.
    int total = 0;
    ListIterator iterador = getIterator(xs);
    while (! atEnd(iterador)) {
        total = total + current(iterador);
        Next(iterador);
    };
    total = total + current(iterador);
    DisposeIterator(iterador);
    return total;
};

void Sucesores(LinkedList xs) {
//Incrementa en uno todos los elementos.
    ListIterator iterador = getIterator(xs);
    while (! atEnd(iterador)) {
        (SetCurrent((current(iterador)) + 1, iterador));
        Next(iterador);
    };
    DisposeIterator(iterador);
};

bool pertenece(int x, LinkedList xs) {
//Indica si el elemento pertenece a la lista.
    ListIterator iterador = getIterator(xs);
    while (! atEnd(iterador) || current(iterador) == x) {
        Next(iterador);
    }
    return(current(iterador) == x);
    DisposeIterator(iterador);
};

int apariciones(int x, LinkedList xs) {
//Indica la cantidad de elementos iguales a x.
    ListIterator iterador = getIterator(xs);
    int contador = 0;
    while (!atEnd(iterador)) {
        if (current(iterador) == x) {
            contador = contador + 1;
        };
    Next(iterador);
    };
    if (current(iterador) == x) {
            contador = contador + 1;
    };
    DisposeIterator(iterador);
    return contador;
};

int minimo(LinkedList xs) {
//Devuelve el elemento más chico de la lista.
    ListIterator iterador = getIterator(xs);
    int elMasChicoHastaAhora = current(iterador);
    while (! atEnd(iterador)) {
        Next(iterador);
        elMasChicoHastaAhora = menorEntre(elMasChicoHastaAhora, current(iterador));
    };
    DisposeIterator(iterador);
    return elMasChicoHastaAhora;
};

LinkedList copy(LinkedList xs) {
//Dada una lista genera otra con los mismos elementos, en el mismo orden.
    LinkedList nuevaLista = nil();
    ListIterator iterador = getIterator(xs);
    while (! atEnd(iterador)) {
        Cons(current(iterador),nuevaLista);
        Next(iterador);
    };
    Cons(current(iterador),nuevaLista);
    DisposeIterator(iterador);
    return nuevaLista;
};