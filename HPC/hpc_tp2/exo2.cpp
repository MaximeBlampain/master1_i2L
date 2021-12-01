#include <iostream>
#include <thread>
#include <mutex>
#include <cmath>


// compilation : g++ -std=c++11 -pthread exo2.cpp -o exo2
// run : ./exo2

#define N 10000000
std::mutex mutex;

// fonction calculant la somme des entiers entre i0 et i1
void somme(int i0, int i1) {
    // lock
    mutex.lock();
    // indique le démarrage et l'id du thread
    std::cout << std::this_thread::get_id() << std::endl;

    double res=0;
    for (int i=i0; i<i1; i++)
        res += i;

    // Sleep 1 sec
    std::this_thread::sleep_for(std::chrono::seconds(1));
    // unlock
    mutex.unlock();
}

int main(int argc, char *argv[]){

    std::cout << "Début du main " << std::endl;
    int nbTh = 10;
    std::thread tab_th[nbTh];

    // lancer les threads
    //std::thread thread1(somme, 0, N/2); // appel non bloquant
    //std::thread thread2(somme, N/2, N); // appel non bloquant

    // lancer les threads
    for(int i=0; i<nbTh ; i++) {
        if (i % 2)
            tab_th[i] = std::thread(somme, 0, N / 2);
        else {
            tab_th[i] = std::thread(somme, N / 2, N);
        }
    }

    std::cout << "Milieu du main " << std::endl;

    //attendre la fin des calculs
    //thread1.join(); // appel bloquant
    //thread2.join(); // appel bloquant

    //attendre la fin des calculs
    for(int i=0; i<nbTh ; i++) {
        tab_th[i].join();
    }
    std::cout << "Fin du main" << std::endl;

    return 1;
}
