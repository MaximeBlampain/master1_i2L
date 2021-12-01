//
// Created by mblampain on 6/24/21.
//
#include <iostream>
#include <thread>
#include <cmath>
#include <chrono>
#include <omp.h>

#define N 100

float total_racine_log = 0,
      total_racine = 0,
      total_sum = 0;

void sum_racine_log(int i0, int i1) {
    #pragma omp parallel
    {
        float sum = 0;
        #pragma omp parallel for
            for (int i = i0; i <= i1; i++) {
                sum += sqrt(i) * log(i);
            }

        // màj de la variable
        #pragma omp atomic
            total_racine_log += sum;
    }
}

void sum_racine(int i0, int i1) {
    #pragma omp parallel
    {
        float sum = 0;
        #pragma omp parallel for
            for(int i = i0; i<=i1; i++) {
                sum += sqrt(i);
            }

        // màj de la variable
        #pragma omp atomic
            total_racine += sum;
    }
}

void sum(int i0, int i1) {
    #pragma omp parallel
    {
        float sum = 0;
        #pragma omp parallel for
            for (int i = i0; i < i1; i++) {
                sum += i;
            }

        // màj de la variable
        #pragma omp atomic
            total_sum += sum;
    }
}

int main(int argc, char *argv[]){

    // start chrono
    double startTime = omp_get_wtime();
    sum( 1, N);
    sum_racine( 1, N);
    sum_racine_log( 1, N);


    // stop chrono
    double endTime = omp_get_wtime();
    std::cout << "temps total -> " << endTime - startTime << " s" << std::endl;

    return 1;
}