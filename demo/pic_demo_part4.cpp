int const nbSteps = 100;

double const step_duration = 0.2;

int const gridSize = 64;

int const nbCells = ((gridSize * gridSize) * gridSize);

int const bagCapacity = 100;

double const charge = 1.;

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

void bag_push(bag &b, particle p) {
  b.items_pos_x[b.nb] = p.pos_x;
  b.items_pos_y[b.nb] = p.pos_y;
  b.items_pos_z[b.nb] = p.pos_z;
  b.items_speed_x[b.nb] = p.speed_x;
  b.items_speed_y[b.nb] = p.speed_y;
  b.items_speed_z[b.nb] = p.speed_z;
  b.nb++;
}

void bag_clear(bag &b) { b.nb = 0; }

void bag_transfer(bag &b1, bag &b2) {
  for (int i = 0; (i < b2.nb); i++) {
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
      bag &b = bagsCur[idCell];
      int nb = b.nb;
      double speed2_x[nb];
      double speed2_y[nb];
      double speed2_z[nb];
      double pos2_x[nb];
      double pos2_y[nb];
      double pos2_z[nb];
      for (int idParticle = 0; (idParticle < nb); idParticle++) {
        speed2_x[idParticle] =
            (b.items_speed_x[idParticle] + (charge * field.x));
        speed2_y[idParticle] =
            (b.items_speed_y[idParticle] + (charge * field.y));
        speed2_z[idParticle] =
            (b.items_speed_z[idParticle] + (charge * field.z));
      }
      for (int idParticle = 0; (idParticle < nb); idParticle++) {
        pos2_x[idParticle] = (b.items_speed_x[idParticle] +
                              (step_duration * speed2_x[idParticle]));
        pos2_y[idParticle] = (b.items_speed_y[idParticle] +
                              (step_duration * speed2_y[idParticle]));
        pos2_z[idParticle] = (b.items_speed_z[idParticle] +
                              (step_duration * speed2_z[idParticle]));
      }
      for (int idParticle = 0; (idParticle < nb); idParticle++) {
        int idCell2 = idCellOfPos(pos2);
        nextCharge[idCell2] += 1.;
        particle p2 = {pos2_z[idParticle],   pos2_y[idParticle],
                       pos2_x[idParticle],   speed2_z[idParticle],
                       speed2_y[idParticle], speed2_x[idParticle]};
        bag &b2 = bagsNext[idCell2];
        int k = b.nb;
        b2.items_pos_x[k] = p2.pos_x;
        b2.items_pos_y[k] = p2.pos_y;
        b2.items_pos_z[k] = p2.pos_z;
        b2.items_speed_x[k] = p2.speed_x;
        b2.items_speed_y[k] = p2.speed_y;
        b2.items_speed_z[k] = p2.speed_z;
        b2.nb++;
      }
      bag_clear(bagsCur[idCell]);
    }
    updateFieldsUsingNextCharge();
    // for (int idCell = 0; (idCell < nbCells); idCell++) {
    //   bag_transfer(bagsCur[idCell], bagsNext[idCell]);
    // }
  }
}