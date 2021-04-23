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
  double pos_x;
  double pos_y;
  double pos_z;
  double speed_x;
  double speed_y;
  double speed_z;
} particle;

typedef struct {
  int nb;
  double items_pos_x[bagCapacity];
  double items_pos_y[bagCapacity];
  double items_pos_z[bagCapacity];
  double items_speed_x[bagCapacity];
  double items_speed_y[bagCapacity];
  double items_speed_z[bagCapacity];
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
        vect speed2;
        speed2.x = ((b->items_speed_x)[idParticle] + field.x);
        speed2.y = ((b->items_speed_y)[idParticle] + field.y);
        speed2.z = ((b->items_speed_z)[idParticle] + field.z);
        vect pos2;
        pos2.x = ((b->items_pos_x)[idParticle] + (step_duration * speed2.x));
        pos2.y = ((b->items_pos_y)[idParticle] + (step_duration * speed2.y));
        pos2.z = ((b->items_pos_z)[idParticle] + (step_duration * speed2.z));
        int idCell2 = idCellOfPos(pos2);
        nextCharge[idCell2] += charge;
        particle p2;
        bag *b2 = (&bagsNext[idCell2]);
        int k = (b->nb);
        (b2->items_pos_x)[k] = pos2.x;
        (b2->items_pos_y)[k] = pos2.y;
        (b2->items_pos_z)[k] = pos2.z;
        (b2->items_speed_x)[k] = speed2.x;
        (b2->items_speed_y)[k] = speed2.y;
        (b2->items_speed_z)[k] = speed2.z;
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
