{
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
    return {((v1.x) + (v2.x)), ((v1.y) + (v2.y)), ((v1.z) + (v2.z))};
  }

  vect vect_mul(double d, vect v) {
    return {(d * (v.x)), (d * (v.y)), (d * (v.z))};
  }

  typedef struct {
    vect pos;
    vect speed;
  } particle;

  typedef struct {
    int nb;
    particle items[bagCapacity];
  } bag;

  void bag_push(bag * b, particle p) {
    set(((((*b).items)[((*b).nb)].pos).x), ((p.pos).x));
    set(((((*b).items)[((*b).nb)].pos).y), ((p.pos).y));
    set(((((*b).items)[((*b).nb)].pos).z), ((p.pos).z));
    set(((((*b).items)[((*b).nb)].speed).x), ((p.speed).x));
    set(((((*b).items)[((*b).nb)].speed).y), ((p.speed).y));
    set(((((*b).items)[((*b).nb)].speed).z), ((p.speed).z));
    operator++(((*b).nb));
  }

  void bag_clear(bag * b) { set(((*b).nb), 0); }

  void bag_transfer(bag * b1, bag * b2) {
    {
      for ({
             const int *i = new int;
             set(i, 0);
           };
           ((*i) < ((*b2).nb)); operator++(i)) {
        {
          {
            const bag **mb = new bag *;
            set(mb, b1);
          }
          {
            const particle *mp = new particle;
            set(mp, ((*b2).items)[(*i)]);
          }
          set(struct_access(
                  struct_access(array_access(struct_access((*mb), items),
                                             (*struct_access((*mb), nb))),
                                pos),
                  x),
              (*struct_access(struct_access(mp, pos), x)));
          set(struct_access(
                  struct_access(array_access(struct_access((*mb), items),
                                             (*struct_access((*mb), nb))),
                                pos),
                  y),
              (*struct_access(struct_access(mp, pos), y)));
          set(struct_access(
                  struct_access(array_access(struct_access((*mb), items),
                                             (*struct_access((*mb), nb))),
                                pos),
                  z),
              (*struct_access(struct_access(mp, pos), z)));
          set(struct_access(
                  struct_access(array_access(struct_access((*mb), items),
                                             (*struct_access((*mb), nb))),
                                speed),
                  x),
              (*struct_access(struct_access(mp, speed), x)));
          set(struct_access(
                  struct_access(array_access(struct_access((*mb), items),
                                             (*struct_access((*mb), nb))),
                                speed),
                  y),
              (*struct_access(struct_access(mp, speed), y)));
          set(struct_access(
                  struct_access(array_access(struct_access((*mb), items),
                                             (*struct_access((*mb), nb))),
                                speed),
                  z),
              (*struct_access(struct_access(mp, speed), z)));
          operator++(struct_access((*mb), nb));
        }
        delete mp;
        delete mb;
      }
      delete i;
    }
    bag_clear(b2);
  }

  { const bag[nbCells] *bagsCur = new bag[nbCells]; }

  { const bag[nbCells] *bagsNext = new bag[nbCells]; }

  { const vect[nbCells] *fields = new vect[nbCells]; }

  { const double[nbCells] *nextCharge = new double[nbCells]; }

  void updateFieldsUsingNextCharge();

  int idCellOfPos(vect pos);

  int main() {
    {
      for ({
             const int *step = new int;
             set(step, 0);
           };
           ((*step) < nbSteps); operator++(step)) {
        {
          for ({
                 const int *idCell = new int;
                 set(idCell, 0);
               };
               ((*idCell) < nbCells); operator++(idCell)) {
            {
              {
                const vect *field = new vect;
                set(field, (*array_access(fields, (*idCell))));
              }
              {
                const bag **b = new bag *;
                set(b, array_access(bagsCur, (*idCell)));
              }
              {
                const int *nb = new int;
                set(nb, (*struct_access((*b), nb)));
              }
              {
                for ({
                       const int *idParticle = new int;
                       set(idParticle, 0);
                     };
                     ((*idParticle) < (*nb)); operator++(idParticle)) {
                  {
                    { const vect *speed2 = new vect; }
                    set(struct_access(speed2, x),
                        ((*struct_access(
                             struct_access(
                                 array_access(struct_access((*b), items),
                                              (*idParticle)),
                                 speed),
                             x)) +
                         (*struct_access(field, x))));
                    set(struct_access(speed2, y),
                        ((*struct_access(
                             struct_access(
                                 array_access(struct_access((*b), items),
                                              (*idParticle)),
                                 speed),
                             y)) +
                         (*struct_access(field, y))));
                    set(struct_access(speed2, z),
                        ((*struct_access(
                             struct_access(
                                 array_access(struct_access((*b), items),
                                              (*idParticle)),
                                 speed),
                             z)) +
                         (*struct_access(field, z))));
                    { const vect *pos2 = new vect; }
                    set(struct_access(pos2, x),
                        ((*struct_access(
                             struct_access(
                                 array_access(struct_access((*b), items),
                                              (*idParticle)),
                                 pos),
                             x)) +
                         (step_duration * (*struct_access(speed2, x)))));
                    set(struct_access(pos2, y),
                        ((*struct_access(
                             struct_access(
                                 array_access(struct_access((*b), items),
                                              (*idParticle)),
                                 pos),
                             y)) +
                         (step_duration * (*struct_access(speed2, y)))));
                    set(struct_access(pos2, z),
                        ((*struct_access(
                             struct_access(
                                 array_access(struct_access((*b), items),
                                              (*idParticle)),
                                 pos),
                             z)) +
                         (step_duration * (*struct_access(speed2, z)))));
                    {
                      const int *idCell2 = new int;
                      set(idCell2, idCellOfPos((*pos2)));
                    }
                    set(array_access(nextCharge, (*idCell2)),
                        ((*array_access(nextCharge, (*idCell2))) + charge));
                    {
                      const bag **b2 = new bag *;
                      set(b2, array_access(bagsNext, (*idCell2)));
                    }
                    {
                      const bag **mb = new bag *;
                      set(mb, (*b2));
                    }
                    set(struct_access(
                            struct_access(
                                array_access(struct_access((*mb), items),
                                             (*struct_access((*mb), nb))),
                                pos),
                            x),
                        (*struct_access(
                            struct_access({(*speed2), (*pos2)}, pos), x)));
                    set(struct_access(
                            struct_access(
                                array_access(struct_access((*mb), items),
                                             (*struct_access((*mb), nb))),
                                pos),
                            y),
                        (*struct_access(
                            struct_access({(*speed2), (*pos2)}, pos), y)));
                    set(struct_access(
                            struct_access(
                                array_access(struct_access((*mb), items),
                                             (*struct_access((*mb), nb))),
                                pos),
                            z),
                        (*struct_access(
                            struct_access({(*speed2), (*pos2)}, pos), z)));
                    set(struct_access(
                            struct_access(
                                array_access(struct_access((*mb), items),
                                             (*struct_access((*mb), nb))),
                                speed),
                            x),
                        (*struct_access(
                            struct_access({(*speed2), (*pos2)}, speed), x)));
                    set(struct_access(
                            struct_access(
                                array_access(struct_access((*mb), items),
                                             (*struct_access((*mb), nb))),
                                speed),
                            y),
                        (*struct_access(
                            struct_access({(*speed2), (*pos2)}, speed), y)));
                    set(struct_access(
                            struct_access(
                                array_access(struct_access((*mb), items),
                                             (*struct_access((*mb), nb))),
                                speed),
                            z),
                        (*struct_access(
                            struct_access({(*speed2), (*pos2)}, speed), z)));
                    operator++(struct_access((*mb), nb));
                  }
                  delete mb;
                  delete b2;
                  delete idCell2;
                  delete pos2;
                  delete speed2;
                }
                delete idParticle;
              }
              bag_clear(array_access(bagsCur, (*idCell)));
            }
            delete nb;
            delete b;
            delete field;
          }
          delete idCell;
        }
        updateFieldsUsingNextCharge();
        {
          for ({
                 const int *idCell = new int;
                 set(idCell, 0);
               };
               ((*idCell) < nbCells); operator++(idCell)) {
            bag_transfer(array_access(bagsCur, (*idCell)),
                         array_access(bagsNext, (*idCell)));
          }
          delete idCell;
        }
      }
      delete step;
    }
  }
  delete nextCharge;
  delete fields;
  delete bagsNext;
  delete bagsCur;
}
