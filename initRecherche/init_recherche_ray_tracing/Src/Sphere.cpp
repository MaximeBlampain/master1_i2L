#include "Sphere.hpp"
#include <cmath>

#define EPSILON 0.0001

Sphere::Sphere() : Objet(){
  rayon = 1.0;
}


Sphere::Sphere(float xc, float yc, float zc, float r, Materiau m) : Objet(m), centre(xc, yc, zc) {
  rayon = r;
}

Sphere::~Sphere(){}


bool Sphere::intersecte(const Rayon& r, Intersection& inter){
    float xC = this->centre.X;
    float yC = this->centre.Y;
    float zC = this->centre.Z;

    float xO = r.origine.X;
    float yO = r.origine.Y;
    float zO = r.origine.Z;

    float xD = r.direction.dx;
    float yD = r.direction.dy;
    float zD = r.direction.dz;

    float a = pow(xD,2) + pow(yD,2) + pow(zD,2);
    float b = 2 * (xD * (xO - xC) + yD * (yO - yC) + zD * (zO - zC));
    float c = pow(xO - xC, 2) + pow(yO - yC, 2) + pow(zO - zC, 2) - pow(rayon,2);

    float delta = pow(b,2) - 4 * a * c;

    if (delta < 0) return false;
    else if(delta == 0){
        float t = -b / (2 * a);
        if(t > EPSILON){
            float p_x = xO + t * xD;
            float p_y = yO + t * yD;
            float p_z = zO + t * zD;

            inter = Intersection(Point(p_x, p_y, p_z), this, t);
            return true;
        }
        return false;
    } else if (delta > 0) {
        float t1 = (-b + sqrt(delta)) / 2 * a;
        float t2 = (-b - sqrt(delta)) / 2 * a;
        float t = min(t1,t2);

        if( t > EPSILON){
            float p_x = xO + t * xD;
            float p_y = yO + t * yD;
            float p_z = zO + t * zD;

            inter = Intersection(Point(p_x, p_y, p_z), this, t);
            return true;
        }
        return false;
    }
  return false;
}


bool Sphere::coupe(const Rayon& r){
    float xC = this->centre.X;
    float yC = this->centre.Y;
    float zC = this->centre.Z;

    float xO = r.origine.X;
    float yO = r.origine.Y;
    float zO = r.origine.Z;

    float xD = r.direction.dx;
    float yD = r.direction.dy;
    float zD = r.direction.dz;

    float a = pow(xD,2) + pow(yD,2) + pow(zD,2);
    float b = 2 * (xD * (xO - xC) + yD * (yO - yC) + zD * (zO - zC));
    float c = pow(xO - xC, 2) + pow(yO - yC, 2) + pow(zO - zC, 2) - pow(rayon,2);

    float delta = pow(b,2) - 4 * a * c;

    if (delta < 0) return false;
    else if(delta == 0){
        float t = -b / (2 * a);
        if(t > EPSILON) return true;

        return false;
    } else if (delta > 0) {
        float t1 = (-b + sqrt(delta)) / 2 * a;
        float t2 = (-b - sqrt(delta)) / 2 * a;
        float t = min(t1,t2);

        if( t > EPSILON) return true;

        return false;
    }
  return false;
}

ostream& operator<<(ostream & sortie, Sphere & s){
  // affichage de l'équation de la sphère
  sortie << "Sphere : de rayon " << s.rayon << ", de centre ";
  sortie << s.centre;
  // affichage du matériau de la sphère
  Objet *po = &s;
  sortie << *po << flush;
  return sortie;
}

Vecteur Sphere::getNormale(const Point &p){
  // la normale à la sphère au point P est égale
  // au vecteur CP, avec C le centre de la sphère
  Vecteur n(p.X-centre.X,p.Y-centre.Y, p.Z-centre.Z);
  n.normaliser();
  return n;
}


void Sphere::affiche(ostream& out) {
  out << "Sphere : de rayon " << rayon << ", de centre ";
  out << centre;
  Objet::affiche(out);
}

