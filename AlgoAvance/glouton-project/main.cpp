#include <iostream>
#include "Game.h"

int main() {
    // Pour que le programme trouve l'instance en question, il faut noter le chemin entier et pas juste le chemin relatif au dossier
    std::cout << "\ndefault order : " << std::endl;
    Game game5("/Users/nfourny/CLionProjects/Projet-Glouton-2020/instances/instance.dat");

    game5.placementDefault();
    game5.eval();
    game5.print();

    std::cout << "\nMax Aire : " << std::endl;
    Game game("/Users/nfourny/CLionProjects/Projet-Glouton-2020/instance.dat");

    game.variantesGloutons(1);
    game.placementDefault();
    game.eval();
    game.print();

    std::cout << "\nMax encombrement : " << std::endl;

    Game game2("/Users/nfourny/CLionProjects/Projet-Glouton-2020/instance.dat");
    game2.variantesGloutons(2);
    game2.placementDefault();
    game2.eval();
    game2.print();

    std::cout << "\nRandom order : "<< std::endl;

    Game game3("/Users/nfourny/CLionProjects/Projet-Glouton-2020/instance.dat");
    game3.variantesGloutons(3);
    game3.placementDefault();
    game3.eval();
    game3.print();

    std::cout << "\nAutre variante : Plus petite aire à la plus grande "<< std::endl;

    Game game4("/Users/nfourny/CLionProjects/Projet-Glouton-2020/instance.dat");
    game4.variantesGloutons(4);
    game4.placementDefault();
    game4.eval();
    game4.print();

    return 0;
}
