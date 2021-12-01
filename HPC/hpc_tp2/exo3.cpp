#include <iostream>
#include <thread>
#include <mutex>
#include <cmath>
#include <chrono>

// compilation : g++ -std=c++11 -pthread exo3.cpp -o exo3
// run : ./exo3

#define N 100000000

// fonction calculant la somme des entiers entre i0 et i1
void somme(int i0, int i1) {
    auto start = std::chrono::system_clock::now();

    double res=0;
    for (int i=i0; i<i1; i++)
        res += i;

    auto end = std::chrono::system_clock::now();
    std::chrono::duration<double> diff = end-start;

    // affichage résultat
    std::cout << "somme - res = " << res << " en " << diff.count() << " s" << std::endl;
}

// fonction calculant la somme des racines carrés des entiers entre nbStart et nbEnd
void square(int nbStart, int nbEnd){
    auto start = std::chrono::system_clock::now();

    double res=0;
    for (int i=nbStart; i<nbEnd; i++)
        res += sqrt(i);

    auto end = std::chrono::system_clock::now();
    std::chrono::duration<double> diff = end-start;

    // affichage résultat
    std::cout << "somme racines - res = " << res << " en " << diff.count() << " s" << std::endl;
}

// fct calculant la somme des sqrt * log des entiers entre nbStart et nbEnd
void square_log(int nbStart, int nbEnd){
    auto start = std::chrono::system_clock::now();

    double res=0;
    for (int i=nbStart; i<nbEnd; i++)
        res += sqrt(i) * log(i);

    auto end = std::chrono::system_clock::now();
    std::chrono::duration<double> diff = end-start;

    // affichage résultat
    std::cout << "somme (racines x log) - res = " << res << " en " << diff.count() << " s" << std::endl;
}

void do_three(int nbStart, int nbEnd) {
    somme(nbStart, nbEnd);
    square(nbStart, nbEnd);
    square_log(nbStart+1, nbEnd+1);
}

int main(int argc, char *argv[]){

    std::cout << "Début du main " << std::endl;

    auto start = std::chrono::system_clock::now();
    // lancer les threads
    std::thread th(do_three, 0, N);
    th.join();

    auto end = std::chrono::system_clock::now();
    std::chrono::duration<double> diff = end-start;
    std::cout << "tps de calcul sequentiel : " << diff.count() << " s" << std::endl;

    start = std::chrono::system_clock::now();

    std::thread thread1(somme, 0, N); // appel non bloquant
    std::thread thread2(square, 0, N); // appel non bloquant
    std::thread thread3(square_log, 1, N+1); // appel non bloquant

    //attendre la fin des calculs
    thread1.join(); // appel bloquant
    thread2.join(); // appel bloquant
    thread3.join(); // appel bloquant

    auto end1 = std::chrono::system_clock::now();
    std::chrono::duration<double> diff1 = end1-start;
    std::cout << "tps de calcul parallèle : " << diff1.count() << " s" << std::endl;

    std::cout << "Fin du main" << std::endl;

    return 1;
}
