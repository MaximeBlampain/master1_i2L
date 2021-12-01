#include "Triangle.hpp"

#define EPSILON 0.0001

Triangle::Triangle() : Objet(){
  s[0].set(-1, 0, -1);
  s[1].set(1, 0, -1);
  s[2].set(0, 0, 1);
  n.set(0, 0, 1);
}

Triangle::Triangle(const Point p[3], Materiau m) : Objet(m) {
  for(int i=0; i<3; i++)
    s[i] = p[i];

  // calcul de la normale à partir du produit vectoriel AB^AC
  Vecteur v_AB = Vecteur(s[0], s[1]);
  Vecteur v_AC = Vecteur(s[0], s[2]);
  Vecteur pdt_vect = pdt_vect.cross(v_AB, v_AC);
  // cette normale doit ensuite être normalisée...
  pdt_vect.normaliser();
  n = pdt_vect;
}

Triangle::~Triangle(){}


bool Triangle::intersecte(const Rayon& r, Intersection& inter){
    // A
    float xA = s[0].X;
    float yA = s[0].Y;
    float zA = s[0].Z;
    // B
    float xB = s[1].X;
    float yB = s[1].Y;
    float zB = s[1].Z;
    // C
    float xC = s[2].X;
    float yC = s[2].Y;
    float zC = s[2].Z;
    // P
    float xO = r.origine.X;
    float yO = r.origine.Y;
    float zO = r.origine.Z;
    // D
    float xD = r.direction.dx;
    float yD = r.direction.dy;
    float zD = r.direction.dz;
    // N
    float xN = n.dx;
    float yN = n.dy;
    float zN = n.dz;

    // d = aX + bY + cZ
    float d = xN*xA + yN*yA + zN*zA;
    // Si n.d =0  -> Parallèle -> false
    if( (n * r.direction) == 0) return false;
    // t = d(eq plan) - n.P  /  n.d(dir rayon)
    float t = d - (xN*xO + yN*yO + zN*zO) / (n * r.direction);

    if(t < EPSILON) return false;

    // point Q -> Q(t) = P + t.d
    float xQ = xO + t * xD;
    float yQ = yO + t * yD;
    float zQ = zO + t * zD;

    // BA
    float x_BA = xB - xA;
    float y_BA = yB - yA;
    float z_BA = zB - zA;
    Vecteur v_BA = Vecteur(x_BA,y_BA,z_BA);
    // QA
    float x_QA = xQ - xA;
    float y_QA = yQ - yA;
    float z_QA = zQ - zA;
    Vecteur v_QA = Vecteur(x_QA,y_QA,z_QA);
    // Produit vectoriel BAQA
    Vecteur pdt_vect_BAQA = Vecteur(v_BA.cross(v_BA, v_QA));
    float pt_BAQA = pdt_vect_BAQA.operator*(n);
    // CB
    float x_CB = xC - xB;
    float y_CB = yC - yB;
    float z_CB = zC - zB;
    Vecteur v_CB = Vecteur(x_CB,y_CB,z_CB);
    // QB
    float x_QB = xQ - xB;
    float y_QB = yQ - yB;
    float z_QB = zQ - zB;
    Vecteur v_QB = Vecteur(x_QB,y_QB,z_QB);
    // Produit vectoriel CBQB
    Vecteur pdt_vect_CBQB = Vecteur(v_CB.cross(v_CB, v_QB));
    float pt_CBQB = pdt_vect_CBQB.operator*(n);
    // AC
    float x_AC = xA - xC;
    float y_AC = yA - yC;
    float z_AC = zA - zC;
    Vecteur v_AC = Vecteur(x_AC,y_AC,z_AC);
    // QC
    float x_QC = xQ - xC;
    float y_QC = yQ - yC;
    float z_QC = zQ - zC;
    Vecteur v_QC = Vecteur(x_QC,y_QC,z_QC);
    // Produit vectoriel ACQC
    Vecteur pdt_vect_ACQC = Vecteur(v_AC.cross(v_AC, v_QC));
    float pt_ACQC = pdt_vect_ACQC.operator*(n);



    // Si Q n'est pas dans le triangle ABC
    if(pt_BAQA < 0 || pt_CBQB < 0 || pt_ACQC < 0) return false;

    inter = Intersection(Point(xQ, yQ, zQ), this, t);
    return true;
}

bool Triangle::coupe(const Rayon& r){
    // A
    float xA = s[0].X;
    float yA = s[0].Y;
    float zA = s[0].Z;
    // B
    float xB = s[1].X;
    float yB = s[1].Y;
    float zB = s[1].Z;
    // C
    float xC = s[2].X;
    float yC = s[2].Y;
    float zC = s[2].Z;
    // P
    float xO = r.origine.X;
    float yO = r.origine.Y;
    float zO = r.origine.Z;
    // D
    float xD = r.direction.dx;
    float yD = r.direction.dy;
    float zD = r.direction.dz;
    // N
    float xN = n.dx;
    float yN = n.dy;
    float zN = n.dz;

    // d = aX + bY + cZ
    float d = xN*xA + yN*yA + zN*zA;
    // t = d(eq plan) - n.P  /  n.d(dir rayon)
    float t = d - (xN*xO + yN*yO + zN*zO) / (n * r.direction);

    // Si n.d =0  -> Parallèle -> false
    if( (n * r.direction) == 0 || t < EPSILON) return false;

    // point Q -> Q(t) = P + t.d
    float xQ = xO + t * xD;
    float yQ = yO + t * yD;
    float zQ = zO + t * zD;
    Vecteur v_Q = Vecteur(xQ,yQ,zQ);
    // BA
    float x_BA = xB - xA;
    float y_BA = yB - yA;
    float z_BA = zB - zA;
    Vecteur v_BA = Vecteur(x_BA,y_BA,z_BA);
    // QA
    float x_QA = xQ - xA;
    float y_QA = yQ - yA;
    float z_QA = zQ - zA;
    Vecteur v_QA = Vecteur(x_QA,y_QA,z_QA);
    // Produit vectoriel BAQA
    Vecteur pdt_vect_BAQA = Vecteur(v_BA.cross(v_BA, v_QA));
    float pt_BAQA = pdt_vect_BAQA.operator*(n);
    // CB
    float x_CB = xC - xB;
    float y_CB = yC - yB;
    float z_CB = zC - zB;
    Vecteur v_CB = Vecteur(x_CB,y_CB,z_CB);
    // QB
    float x_QB = xQ - xB;
    float y_QB = yQ - yB;
    float z_QB = zQ - zB;
    Vecteur v_QB = Vecteur(x_QB,y_QB,z_QB);
    // Produit vectoriel CBQB
    Vecteur pdt_vect_CBQB = Vecteur(v_CB.cross(v_CB, v_QB));
    float pt_CBQB = pdt_vect_CBQB.operator*(n);
    // AC
    float x_AC = xA - xC;
    float y_AC = yA - yC;
    float z_AC = zA - zC;
    Vecteur v_AC = Vecteur(x_AC,y_AC,z_AC);
    // QC
    float x_QC = xQ - xC;
    float y_QC = yQ - yC;
    float z_QC = zQ - zC;
    Vecteur v_QC = Vecteur(x_QC,y_QC,z_QC);
    // Produit vectoriel ACQC
    Vecteur pdt_vect_ACQC = Vecteur(v_AC.cross(v_AC, v_QC));
    float pt_ACQC = pdt_vect_ACQC.operator*(n);


    // Si Q n'est pas dans le triangle ABC
    if(pt_BAQA < 0 || pt_CBQB < 0 || pt_ACQC < 0) return false;

    return true;
}


Vecteur Triangle::getNormale(const Point &p){
  return n;
}

ostream& operator<<(ostream & sortie, Triangle & t){
  sortie << "triangle : ";
  for(int i=0; i<3; i++)
    sortie << t.s[i] << " - ";
  sortie << endl;

  return sortie;
    
}

void Triangle::affiche(ostream& out){
  out << "triangle : ";
  for(int i=0; i<3; i++)
    out << s[i] << " - ";
  out << endl;
}

