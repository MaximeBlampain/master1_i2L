//
// Created by mblampain on 6/27/21.
//

#include <iostream>
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
    double pi;
    double realPi = 3.141592653589793238462643;
    double nbPoints = 1000*1000*10000;

    while(i<nbPoints){
        double x = ((double) rand() / RAND_MAX * 2) -1;
        double y = ((double) rand() / RAND_MAX * 2) -1;
        double xCarreYCarre = pow(x, 2) + pow(y,2);
        //std::cout << "x -> " << x << std::endl;
        //std::cout << "y -> " << y << std::endl;
        //std::cout << "xCarreYCarre -> " << xCarreYCarre << std::endl;

        if(xCarreYCarre < 1) nbPointDansCercle +=1;

        i+=1;
        //std::cout << "i -> " << i << std::endl;
        //std::cout << "nbPointDansCercle -> " << nbPointDansCercle << std::endl;
        float P = nbPointDansCercle / i;
        //std::cout << "P -> " << P << std::endl;
        pi = P * 4;
        double arrondisPi = realPi - pi;
        //std::cout << "arrondisPi -> " << arrondisPi << std::endl;

        if(arrondisPi < 0)  arrondisPi = arrondisPi * -1;

        if(arrondisPi < pow(10, -6)) {
            std::cout << "good precision : " << arrondisPi << std::endl;
            break;
        }
    }

    std::cout << "nbPointDansCercle -> " << nbPointDansCercle << std::endl;
    std::cout << "pi -> " << pi << std::endl;


    return 0;
}