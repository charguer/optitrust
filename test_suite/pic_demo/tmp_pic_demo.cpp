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
  (b->items)[(b->nb)].pos.x = p.pos.x;
  (b->items)[(b->nb)].pos.y = p.pos.y;
  (b->items)[(b->nb)].pos.z = p.pos.z;
  (b->items)[(b->nb)].speed.x = p.speed.x;
  (b->items)[(b->nb)].speed.y = p.speed.y;
  (b->items)[(b->nb)].speed.z = p.speed.z;
  (b->nb)++;
}

void bag_clear(bag *b) { (b->nb) = 0; }

void bag_transfer(bag *b1, bag *b2) {
  for (int i = 0; (i < (b2->nb)); i++) {
    bag *mb = b1;
    particle mp = (b2->items)[i];
    (mb->items)[(mb->nb)].pos.x = mp.pos.x;
    (mb->items)[(mb->nb)].pos.y = mp.pos.y;
    (mb->items)[(mb->nb)].pos.z = mp.pos.z;
    (mb->items)[(mb->nb)].speed.x = mp.speed.x;
    (mb->items)[(mb->nb)].speed.y = mp.speed.y;
    (mb->items)[(mb->nb)].speed.z = mp.speed.z;
    (mb->nb)++;
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
        vect speed2;
        speed2.x = ((b->items)[idParticle].speed.x + field.x);
        speed2.y = ((b->items)[idParticle].speed.y + field.y);
        speed2.z = ((b->items)[idParticle].speed.z + field.z);
        vect pos2;
        pos2.x = ((b->items)[idParticle].pos.x + (step_duration * speed2.x));
        pos2.y = ((b->items)[idParticle].pos.y + (step_duration * speed2.y));
        pos2.z = ((b->items)[idParticle].pos.z + (step_duration * speed2.z));
        int idCell2 = idCellOfPos(pos2);
        nextCharge[idCell2] += charge;
        bag *b2 = (&bagsNext[idCell2]);
        bag *mb = b2;
        (mb->items)[(mb->nb)].pos.x = {speed2, pos2}.pos.x;
        (mb->items)[(mb->nb)].pos.y = {speed2, pos2}.pos.y;
        (mb->items)[(mb->nb)].pos.z = {speed2, pos2}.pos.z;
        (mb->items)[(mb->nb)].speed.x = {speed2, pos2}.speed.x;
        (mb->items)[(mb->nb)].speed.y = {speed2, pos2}.speed.y;
        (mb->items)[(mb->nb)].speed.z = {speed2, pos2}.speed.z;
        (mb->nb)++;
      }
      bag_clear((&bagsCur[idCell]));
    }
    updateFieldsUsingNextCharge();
    for (int idCell = 0; (idCell < nbCells); idCell++) {
      bag_transfer((&bagsCur[idCell]), (&bagsNext[idCell]));
    }
  }
}
