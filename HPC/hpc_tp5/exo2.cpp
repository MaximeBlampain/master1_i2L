//
// Created by mblampain on 6/27/21.
//

#include <iostream>
#include <mpi.h>
#include <math.h>
#include <time.h>
#include <limits>


int main(int argc, char *argv[]){

    //random seed
    srand(time(NULL));

    typedef std::numeric_limits<double>dbl;
    std::cout.precision(dbl::max_digits10);

    double  nbPointDansCercle = 0;
    double i=0;
    double pi, resultPi;
    double realPi = 3.141592653589793238462643;
    double nbPoints = 10000000000;
    int nbProcess, myID;
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &nbProcess);
    MPI_Comm_rank(MPI_COMM_WORLD, &myID);
    while(i<nbPoints){
        MPI_Bcast(&i, 1, MPI_DOUBLE, 0, MPI_COMM_WORLD);
        double x = ((double) rand() / RAND_MAX * 2) -1;
        double y = ((double) rand() / RAND_MAX * 2) -1;
        double xCarreYCarre = pow(x, 2) + pow(y,2);

        if(xCarreYCarre < 1) nbPointDansCercle +=1;

        i+=1;
        MPI_Reduce(&pi, &resultPi, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);
        float P = nbPointDansCercle / i;
        pi = P * 4;
        double arrondisPi = realPi - pi;

        if(arrondisPi < 0)  arrondisPi = arrondisPi * -1;

        if(arrondisPi < pow(10, -6)) {
            std::cout << "good precision : " << arrondisPi << std::endl;
            break;
        }
    }
    MPI_Finalize();
    std::cout << "nbPointDansCercle -> " << nbPointDansCercle << std::endl;
    std::cout << "pi -> " << pi << std::endl;


    return 0;
}