#include <thread>
#include "Camera.hpp"
#include "Rayon.hpp"
#include <chrono>
#include <mutex>

// taille hauteur et largeur d'une zone
#define LARGZONE 32
#define HAUTZONE 32
std::mutex mtx;

Camera::Camera(){
  position = Point(0.0, 0.0, 2.0);;
  cible = Point(0.0, 0.0, 0.0);
  distance = 2.0;
}

Camera::~Camera(){}

void Camera::genererImage(const Scene& sc, Image& im, int profondeur){

  // Calcul des dimensions d'un pixel par rapport
  // à la résolution de l'image - Les pixels doivent être carrés
  // pour éviter les déformations de l'image.
  // On fixe :
  // - les dimensions en largeur de l'écran seront comprises dans [-1, 1]
  // - les dimensions en hauteur de l'écran soront comprises dans [-H/L, H/L]
  float cotePixel = 2.0/im.getLargeur();

  // Pour chaque pixel
  for(int i=0; i<im.getLargeur(); i++){
    for(int j=0; j<im.getHauteur(); j++){
      
      // calcul des coordonnées du centre du pixel
      float milieuX = -1 + (i+0.5f)*cotePixel;
      float milieuY =  (float)im.getHauteur()/(float)im.getLargeur() - (j+0.5f)*cotePixel;
 
      Point centre(milieuX, milieuY, 0);
      
      // Création du rayon
      Vecteur dir(position, centre);
      dir.normaliser();
      Rayon ray(position, dir);
      
      // Lancer du rayon primaire
      Intersection inter;
      if(sc.intersecte(ray, inter)){
	    im.setPixel(i, j, inter.getCouleur(sc, position, profondeur));
      }
      else
	    im.setPixel(i, j, sc.getFond());

    }// for j

  }// for i
}

static zone zoneSuivante(const Image &im){
    static int n = 0;
    static int nbCarreLigne = (int)(im.getLargeur() / LARGZONE);
    static int nbCarreColonne = (int)(im.getHauteur() / HAUTZONE);

    int ligne = (int)(n / nbCarreLigne);
    int colonne = n % nbCarreColonne;

    // Si l'image a été parcourue entièrement
    if(ligne >= nbCarreLigne){
        zone empty;
        empty.x = -1;
        empty.y = -1;
        empty.hauteur = -1;
        empty.largeur = -1;

        return empty;
    }

    zone carres [nbCarreColonne][nbCarreLigne];

    for(int i=0 ; i<nbCarreColonne ; i++){
        for(int j=0 ; j<nbCarreLigne ; j++){
            zone carre;

            carre.x = j * LARGZONE;
            carre.y = i * HAUTZONE;
            // si dernier carré de la ligne, ajuster les pixels manquants
            if(j == nbCarreLigne -1){
                int restePixel = im.getLargeur() - (LARGZONE * nbCarreLigne);
                carre.largeur = carre.x + LARGZONE + restePixel;
            }else{
                carre.largeur = carre.x + LARGZONE;
            }
            // si dernier carré de la colonne, ajsuter les pixels manquants
            if(i == nbCarreColonne -1){
                int restePixel = im.getHauteur() - (HAUTZONE * nbCarreColonne);
                carre.hauteur = carre.y + HAUTZONE + restePixel;
            }else{
                carre.hauteur = carre.y + HAUTZONE;
            }
            carres[i][j] = carre;
        }
    }

    n+=1;
    return carres[ligne][colonne];
}

static void calculeZone(const Scene &sc, Image &im, int profondeur, const Point &position) {
    bool isEmpty = false;
    int nbZoneCalcule = 0;
    zone carre;

    auto start = std::chrono::system_clock::now();
    do {
        carre = zoneSuivante(std::ref(im));
        if(carre.x == -1 || carre.y == -1 || carre.hauteur == -1 || carre.largeur == -1)
            isEmpty = true;
        else {

            float cotePixel = 2.0/im.getLargeur();
            nbZoneCalcule ++;
            auto startCarre = std::chrono::system_clock::now();

            // lock
            mtx.lock();

            // Pour chaque pixel
            for(int i=carre.x; i<carre.largeur ; i++){
                for(int j=carre.y; j<carre.hauteur ; j++){

                    // calcul des coordonnées du centre du pixel
                    float milieuX = -1 + (i+0.5f)*cotePixel;
                    float milieuY =  (float)im.getHauteur()/(float)im.getLargeur() - (j+0.5f)*cotePixel;

                    Point centre(milieuX, milieuY, 0);

                    // Création du rayon
                    Vecteur dir(position, centre);
                    dir.normaliser();
                    Rayon ray(position, dir);

                    // Lancer du rayon primaire
                    Intersection inter;
                    if(sc.intersecte(ray, inter)){
                        im.setPixel(i, j, inter.getCouleur(sc, position, profondeur));
                    }
                    else
                        im.setPixel(i, j, sc.getFond());
                }// for j

            }// for i

            // unlock
            mtx.unlock();

            auto endCarre = std::chrono::system_clock::now();
            std::chrono::duration<double> diffCarre = endCarre-startCarre;
            std::cout << "tps de calcul de la zone : " << diffCarre.count() << " s" << std::endl;
        }

    }while(!isEmpty);

    auto end = std::chrono::system_clock::now();
    std::chrono::duration<double> diff = end-start;
    std::cout << "THREAD " << std::this_thread::get_id() << std::endl;
    std::cout << "Nombre de zone calculée par le thread : " << nbZoneCalcule << std::endl;
    std::cout << "tps de calcul du thread : " << diff.count() << " s" << std::endl;

}

void Camera::genererImageParallele(const Scene& sc, Image& im, int profondeur, int nbThreads) {
    std::thread threads[nbThreads];

    for(int i=0 ; i<nbThreads; i++) {
        threads[i] = std::thread(calculeZone, std::ref(sc),std::ref(im), profondeur, Camera::position);
    }
    for (int i = 0; i < nbThreads; i++) {
        threads[i].join();
    }
}
ostream& operator<<(ostream& out, const Camera& c){

  out << " position = " << c.position << " - cible = " << c.cible;
  out <<  " - distance = " << c.distance << flush;
  return out;
}
