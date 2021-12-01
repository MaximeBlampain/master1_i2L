#include <thread>
#include "Camera.hpp"
#include "Rayon.hpp"
#include <chrono>
#include <mutex>


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
static void calculeZone(const Scene &sc, Image &im, int profondeur, const zone &area, const Point &position) {

    // lock
    mtx.lock();
    auto start = std::chrono::system_clock::now();

    float cotePixel = 2.0/im.getLargeur();

    // Pour chaque pixel
    for(int i=area.x; i<area.largeur ; i++){
        for(int j=area.y; j<area.hauteur ; j++){

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
    auto end = std::chrono::system_clock::now();
    std::chrono::duration<double> diff = end-start;
    std::cout << "THREAD " << std::this_thread::get_id() << std::endl;
    std::cout << "tps de calcul du thread : " << diff.count() << " s" << std::endl;

}

void Camera::genererImageParallele(const Scene& sc, Image& im, int profondeur, int nbThreads) {
    std::thread threads[nbThreads];

    int hauteurBande = (int)(im.getHauteur() / nbThreads);

    for(int i=0 ; i<nbThreads; i++) {
        int y = i * hauteurBande;

        zone bande;
        bande.x = 0;
        bande.y = y;
        // if last thread, add missing pixels
        if (i == (nbThreads - 1)) {
            int restePixel = im.getHauteur() - (hauteurBande * nbThreads);
            bande.hauteur = y + hauteurBande + restePixel;
        } else {
            bande.hauteur = y + hauteurBande;
        }
        bande.largeur = im.getLargeur();

        threads[i] = std::thread(calculeZone, std::ref(sc),std::ref(im), profondeur, bande, Camera::position);
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
