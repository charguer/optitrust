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
    set(((*b).items)[((*b).nb)], p);
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
        bag_push(b1, ((*b2).items)[(*i)]);
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
                    {
                      const particle *p = new particle;
                      set(p, (*array_access(struct_access((*b), items),
                                            (*idParticle))));
                    }
                    { const vect *speed2 = new vect; }
                    set(speed2, {((*struct_access(struct_access(p, speed), x)) +
                                  (*struct_access(field, x))),
                                 ((*struct_access(struct_access(p, speed), y)) +
                                  (*struct_access(field, y))),
                                 ((*struct_access(struct_access(p, speed), z)) +
                                  (*struct_access(field, z)))});
                    { const vect *pos2 = new vect; }
                    { const vect *nv2 = new vect; }
                    const double pv1 = step_duration;
                    const vect pv2 = (*speed2);
                    set(nv2,
                        {(pv1 * (pv2.x)), (pv1 * (pv2.y)), (pv1 * (pv2.z))});
                    set(pos2, {((*struct_access(struct_access(p, pos), x)) +
                                (*struct_access(nv2, x))),
                               ((*struct_access(struct_access(p, pos), y)) +
                                (*struct_access(nv2, y))),
                               ((*struct_access(struct_access(p, pos), z)) +
                                (*struct_access(nv2, z)))});
                    {
                      const int *idCell2 = new int;
                      set(idCell2, idCellOfPos((*pos2)));
                    }
                    set(array_access(nextCharge, (*idCell2)),
                        ((*array_access(nextCharge, (*idCell2))) + charge));
                    {
                      const particle *p2 = new particle;
                      set(p2, {(*speed2), (*pos2)});
                    }
                    bag_push(array_access(bagsNext, (*idCell2)), (*p2));
                    {
                      const bag **b2 = new bag *;
                      set(b2, array_access(bagsNext, (*idCell2)));
                    }
                    {
                      const int *k = new int;
                      set(k, (*struct_access((*b), nb)));
                    }
                    set(struct_access(
                            struct_access(
                                array_access(struct_access((*b2), items), (*k)),
                                pos),
                            x),
                        (*struct_access(pos2, x)));
                    set(struct_access(
                            struct_access(
                                array_access(struct_access((*b2), items), (*k)),
                                pos),
                            y),
                        (*struct_access(pos2, y)));
                    set(struct_access(
                            struct_access(
                                array_access(struct_access((*b2), items), (*k)),
                                pos),
                            z),
                        (*struct_access(pos2, z)));
                    set(struct_access(
                            struct_access(
                                array_access(struct_access((*b2), items), (*k)),
                                speed),
                            x),
                        (*struct_access(speed2, x)));
                    set(struct_access(
                            struct_access(
                                array_access(struct_access((*b2), items), (*k)),
                                speed),
                            y),
                        (*struct_access(speed2, y)));
                    set(struct_access(
                            struct_access(
                                array_access(struct_access((*b2), items), (*k)),
                                speed),
                            z),
                        (*struct_access(speed2, z)));
                    operator++(struct_access((*b2), nb));
                  }
                  delete k;
                  delete b2;
                  delete p2;
                  delete idCell2;
                  delete nv2;
                  delete pos2;
                  delete speed2;
                  delete p;
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
