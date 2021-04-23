const double charge = 1.;

const int gridSize = 64;

const int nbCells = ((gridSize * gridSize) * gridSize);

const int bagCapacity = 100;

const double step_duration = 0.2;

const int nbSteps = 100;

typedef struct {
  double x;
  double y;
  double z;
} vect;

vect vect_add(vect v1, vect v2) {
  return {(v1.x + v2.x), (v1.y + v2.y), (v1.z + v2.z)};
}

vect vect_mul(double d, vect v) { return {(d * v.x), (d * v.y), (d * v.z)}; }

typedef struct {
  vect pos;
  vect speed;
} particle;

typedef struct {
  int nb;
  particle items[bagCapacity];
} bag;

void bag_push(bag *b, particle p) {
  (b->items)[(b->nb)] = p;
  (b->nb)++;
}

void bag_clear(bag *b) { (b->nb) = 0; }

void bag_transfer(bag *b1, bag *b2) {
  for (int i = 0; (i < (b2->nb)); i++) {
    bag_push(b1, (b2->items)[i]);
  }
  bag_clear(b2);
}

bag bagsCur[nbCells];

bag bagsNext[nbCells];

vect fields[nbCells];

double nextCharge[nbCells];

void updateFieldsUsingNextCharge();

int idCellOfPos(vect pos);

int main() {
  for (int step = 0; (step < nbSteps); step++) {
    for (int idCell = 0; (idCell < nbCells); idCell++) {
      vect field = fields[idCell];
      bag *b = (&bagsCur[idCell]);
      int nb = (b->nb);
      for (int idParticle = 0; (idParticle < nb); idParticle++) {
        particle p = (b->items)[idParticle];
        vect speed2;
        speed2 = {(p.speed.x + field.x), (p.speed.y + field.y),
                  (p.speed.z + field.z)};
        vect pos2;
        pos2 = {(p.pos.x + (step_duration * speed2.x)),
                (p.pos.y + (step_duration * speed2.y)),
                (p.pos.z + (step_duration * speed2.z))};
        int idCell2 = idCellOfPos(pos2);
        nextCharge[idCell2] += charge;
        particle p2 = {speed2, pos2};
        bag_push((&bagsNext[idCell2]), p2);
        bag *b2 = (&bagsNext[idCell2]);
        int k = (b->nb);
        (b2->items)[k].pos.x = pos2.x;
        (b2->items)[k].pos.y = pos2.y;
        (b2->items)[k].pos.z = pos2.z;
        (b2->items)[k].speed.x = speed2.x;
        (b2->items)[k].speed.y = speed2.y;
        (b2->items)[k].speed.z = speed2.z;
        (b2->nb)++;
      }
      bag_clear((&bagsCur[idCell]));
    }
    updateFieldsUsingNextCharge();
    for (int idCell = 0; (idCell < nbCells); idCell++) {
      bag_transfer((&bagsCur[idCell]), (&bagsNext[idCell]));
    }
  }
}
