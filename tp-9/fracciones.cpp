#include <iostream>
#include "fracciones.h";

// Propósito: construye una fraccion
// Precondición: el denominador no es cero
Fraccion consFraccion(int numerador, int denominador) {
    Fraccion f;
    f.numerador   = numerador;
    f.denominador = denominador;
    return(f);
};

// Propósito: devuelve el numerador
int numerador(Fraccion f) {
    return(f.numerador);
};

// Propósito: devuelve el denominador
int denominador(Fraccion f) {
    return(f.denominador);
};

// Propósito: devuelve el resultado de hacer la división
float division(Fraccion f) {
    return(f.numerador / f.denominador);
};

// Propósito: devuelve una fracción que resulta de multiplicar las fracciones(sin simplificar)
Fraccion multF(Fraccion f1, Fraccion f2) {
    Fraccion f;
    f.numerador   = f1.numerador * f2.numerador;
    f.denominador = f1.denominador * f2.denominador;
    return(f);
};

// Propósito: devuelve una fracción que resulta de simplificar la dada por parámetro
// es un quilombo al pedo.

// Propósito: devuelve la fracción resultante de sumar las fracciones
Fraccion sumF(Fraccion f1, Fraccion f2) {
    Fraccion f;
    f.numerador   = f1.numerador + f2.numerador;
    f.denominador = f1.denominador + f2.denominador;
    return(f);
}; // es mas quilombo en realidad la cuenta matematica.