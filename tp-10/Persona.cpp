#include <iostream>
#include "persona.h";
using namespace std;

struct PersonaSt {
    string nombre;
    int    edad  ;
};

Persona consPersona(string nombre, int edad) {
    PersonaSt* p = new PersonaSt;
    (*p).nombre  = nombre;
    p -> edad    = edad;
};

string nombre(Persona p) {
    return p -> nombre;
};

int edad(Persona p) {
    return p -> edad;
};

void crecer(Persona p) {
    p -> edad = p -> edad + 1;
};

void cambioDeNombre(string nombre, Persona p) {
    p -> nombre = nombre;
};

bool esMayorQueLaOtra(Persona p1, Persona p2) {
    (p1 -> edad) > (p2 -> edad);
};

Persona laQueEsMayor(Persona p1, Persona p2) {
    if (p1 -> edad > p2 -> edad) 
    {  return p1;              }
    else { return p2;}
};

