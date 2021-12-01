#include <immintrin.h>
#include <iostream>
#include <string>

// programme d'évaluation du gain obtenu en utilisant les routines AVX
// pour le calcul de produits scalaires
// utilise les routines AVX et AVX2 (pour les doubles)
// Compilation : g++  -std=c++11 -march=native -O2 ps.cpp -o ps
//     ou      : g++  -std=c++11 -mavx -mavx2 -O2 ps.cpp -o ps



#define NB 10000  // nombre de fois où le produit scalaire est effectué
#define SIZE 80000 // taille des vecteurs utilisés


// -------------------------------------------
// Fonctions d'affichage
// -------------------------------------------
// affiche les 8 réels contenus dans un __m256
void printf(std::string s, __m256 *v);
void printd(std::string s, __m256d *v);


// -------------------------------------------
// Fonctions de calcul
// -------------------------------------------
// calcul des produits scalaires sous forme de vecteurs d'entiers
void ps_int(int size);

// calcul des produits scalaires sous forme de vecteurs de float
void ps_float(int size);

// calcul des produits scalaires sous forme de vecteurs de double
void ps_double(int size);





int main(int argc, char*argv[]){

  ps_float(SIZE);
  ps_double(SIZE);



  return 0;
}

void printf(std::string str, __m256 *value){
    float *ptr = (float*) value;
    std::cout << str << std::endl;
    for(int i = 0 ; i < 8 ; i++){
        std::string index = std::to_string(i);
        std::string s = "Index -> " + index + " |  Value  -> " + std::to_string(ptr[i]);
        std::cout  << s << std::endl;
    }
}

// effectue les ps séquentiel ou avx sur des réels simple précision
// et affiche le temps de calcul correspondants
void ps_float(int size){

  clock_t start;

  // initialisation des deux vecteurs réels
  alignas(32) float *v1, *v2; // alignement sur 32 bits pour AVX

  v1=(float*)_mm_malloc(size*sizeof(float),32);
  v2=(float*)_mm_malloc(size*sizeof(float),32);

  // initialisation avec des valeurs aléatoires dans [0,9]
  for(int i=0; i<size; i++){
    v1[i] = (float)(rand()%10);
    v2[i] = (float)(rand()%10);
  }

  // produit scalaire séquentiel
  start =  clock();

  float somme;
  for(int n=0; n<NB; n++){
    somme=0;
    for(int i=0; i<size; i++) somme += v1[i]*v2[i];
  }

  float t1;
  std::cout << "temps ps float seq = " << (t1=(clock()-start)/(double)CLOCKS_PER_SEC);
  std::cout << " secondes - valeur = " << somme << std::endl;

  // produit scalaire  avx
  start =  clock();

  __m256 _somme, _v1, _v2;

  for(int n=0; n<NB; n++){
      _somme= _mm256_setzero_ps();
      for(int i=0; i<size; i+=8) {
          _v1 = _mm256_load_ps(v1 + i);
          _v2 = _mm256_load_ps(v2 + i);
          // On multiplie _v1 et _v2
          _v1 = _mm256_mul_ps(_v1, _v2);
          // On incrémente le résultat dans _somme
          _somme = _mm256_add_ps(_somme, _v1);
      }
  }

  float finale_somme = 0;
  float *ptr = (float *)&_somme;
  for (int i = 0; i < 8; i++)
      finale_somme += (float)*(ptr + i);

  // affichage du speedup
  float t2;
  std::cout << "temps ps AVX = " << (t2=(clock()-start)/(double)CLOCKS_PER_SEC);
  std::cout << " secondes - valeur = " << finale_somme << std::endl;

  // désallocation mémoire
  _mm_free(v1);
  _mm_free(v2);
}

// double
void printd(std::string str, __m256d *value){
    std::cout<<str<<std::endl;
    double *ptr = (double*)value;
    for (int i=0 ; i<4 ; i++){
        std::string index = std::to_string(i);
        std::string s = "Index -> " + index + " |  Value  -> " + std::to_string(ptr[i]);
        std::cout  << s << std::endl;
    }
}


void ps_double(int size){
    clock_t start;

    // initialisation des deux vecteurs réels
    alignas(64) double *v1, *v2; // alignement sur 64 bits pour AVX
    v1=(double*)_mm_malloc(size*sizeof(double),64);
    v2=(double*)_mm_malloc(size*sizeof(double),64);

    // initialisation avec des valeurs aléatoires dans [0,9]
    for(int i=0; i<size; i++){
        v1[i] = (double)(rand()%10);
        v2[i] = (double)(rand()%10);
    }

    // produit scalaire séquentiel
    start =  clock();
    double somme;
    for(int n=0; n<NB; n++){
        somme=0;
        for(int i=0; i<size; i++) somme += v1[i]*v2[i];
    }
    double t1;
    std::cout << "temps ps double seq = " << (t1=(clock()-start)/(double)CLOCKS_PER_SEC);
    std::cout << " secondes - valeur = " << somme << std::endl;

    // produit scalaire  avx
    start =  clock();
    __m256d _somme, _v1, _v2;
    for(int n=0; n<NB; n++){
        _somme = _mm256_setzero_pd();
        for(int i=0; i<size; i+=4){
            _v1 = _mm256_load_pd(v1 + i);
            _v2 = _mm256_load_pd(v2 + i);
            _v1 = _mm256_mul_pd(_v1, _v2);
            _somme = _mm256_add_pd(_somme, _v1);
        }
    }
    double finale_somme = 0;
    double *ptr = (double *)&_somme;
    for (int i = 0; i < 4; i++)
        finale_somme += (double)*(ptr + i);

    // affichage du speedup
    double t2;
    std::cout << "temps ps double AVX = " << (t2=(clock()-start)/(double)CLOCKS_PER_SEC);
    std::cout << " secondes - valeur = " << finale_somme << std::endl;

    // désallocation mémoire
    _mm_free(v1);
    _mm_free(v2);
}

