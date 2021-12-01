#include <iostream>
#include <thread>
#include <mutex>
#include <cmath>
#include <chrono>

// compilation : g++ -std=c++11 -pthread exo4.cpp -o exo4
// run : ./exo4

#define N 100000000
#define NBT 500

std::mutex mutex;
double  moy_t_th;
double  res_somme, res_square, res_square_log;

// fct qui effectue les 3 types de sommes
void sommes(int i0, int i1) {
    // lock
    mutex.lock();
    auto start = std::chrono::system_clock::now();

    for (int i=i0; i<i1; i++)
        res_somme += i;

    for (int i=i0; i<i1; i++)
        res_square += sqrt(i);

    for (int i=i0; i<i1; i++)
        res_square_log += sqrt(i) * log(i);

    auto end = std::chrono::system_clock::now();
    std::chrono::duration<double> diff = end-start;

    std::cout << "tps de calcul du thread : " << diff.count() << " s" << std::endl;
    moy_t_th+= diff.count();
    // unlock
    mutex.unlock();
}

int main(int argc, char *argv[]){
    std::cout << "Début du main " << std::endl;
    // Initialisation des résultats
    res_somme = 0;
    res_square = 0;
    res_square_log = 0;
    moy_t_th = 0;
    // Init du tableau de thread
    std::thread tab_th[NBT];
    double inter = N/NBT;
    auto start = std::chrono::system_clock::now();
    for(int i=0, n=1; i<NBT, n<N ; i++, n+=inter) {
        tab_th[i] = std::thread(sommes, n, n + inter - 1);
    }

    for(int i=0; i<NBT ; i++) {
        tab_th[i].join();
    }

    auto end = std::chrono::system_clock::now();
    std::chrono::duration<double> diff = end-start;
    /*std::cout << "somme - res = " << res_somme << std::endl;
    std::cout << "somme racines - res = " << res_square << std::endl;
    std::cout << "somme (racines x log) - res = " << res_square_log << std::endl;
    */std::cout << "tps de calcul parrallèle : " << diff.count() << " s" << std::endl;
    double moy = moy_t_th / NBT;
    std::cout << "tps moyen par thread : " << moy << " s" << std::endl;
    std::cout << "Fin du main" << std::endl;

    return 1;
}
