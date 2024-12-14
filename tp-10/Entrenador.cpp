#include <iostream>
using namespace std;
#include "Entrenador.h";

struct EntrenadorSt {
    string nombre;
    Pokemon* pokemon;
    int cantPokemon;
};

Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemon) {
    EntrenadorSt* e  = new EntrenadorSt;
    e -> nombre      = nombre;
    e -> pokemon     = new pokemon[cantidad];
    e -> cantPokemon = cantidad;
    return e
};

string nombreDeEntrenador(Entrenador e) {
    return e -> nombre;
};

int cantidadDePokemon(Entrenador e) {
    return e -> cantPokemon;
};


int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e) {
    return cantidadDePokemonDeTipoEn_ConCantMaxima_(e -> pokemon, tipo, e -> cantPokemon);
};

int cantidadDePokemonDeTipoEn_(Pokemon* dirArrayP, TipoDePokemon tipo, int cantMax) {
    int n = 0;
    for ( i=0, i<cantMax, i++) {
        if (tipoDePokemon(dirArrayP[i])) == tipo) {
            n = n+1;
        }
    }
    return n;
};


Pokemon pokemonNro(int i, Entrenador e) {
    return e -> pokemon[i-1];
};


bool leGanaATodos(Entrenador e1, Entrenador e2) {
    return cadaUnoDeEstosPokemon_EsSuperadoPorAlgunoDeEstos_(e2 -> pokemon, e1 -> pokemon, e2 -> cantPokemon, e1 -> cantPokemon);
};

bool cadaUnoDeEstosPokemon_EsSuperadoPorAlgunoDeEstos_(Pokemon* p1, Pokemon* p2, int n, int cantPokemon1) {
    for (n > 1, algunoDeEstosPokemon_SuperaAEste_(p1, p2[n-1], cantPokemon1 )) {
        n = n--;
    };
    return algunoDeEstosPokemon_SuperaAEste_(p1, p2[n-1], cantPokemon1);
};

bool algunoDeEstosPokemon_SuperaAEste_(Pokemon* ps, Pokemon* p, int cantPokemonPs) {
    for (i=1, i<cantPokemonPs, not (superaA((ps[i]),p))) {
        i = i++;
    }
    return superaA((ps[i]),p);
};