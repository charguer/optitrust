{
  const int nbSteps = 100;

  const double step_duration = 0.2;

  const int gridSize = 64;

  const int nbCells = ((gridSize * gridSize) * gridSize);

  const int bagCapacity = 100;

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
                    set(speed2, vect_add((*struct_access(p, speed)), (*field)));
                    {
                      const vect *pos2 = new vect;
                      set(pos2, vect_add((*struct_access(p, pos)),
                                         vect_mul(step_duration, (*speed2))));
                    }
                    {
                      const int *idCell2 = new int;
                      set(idCell2, idCellOfPos((*pos2)));
                    }
                    set(array_access(nextCharge, (*idCell2)),
                        ((*array_access(nextCharge, (*idCell2))) + 1.));
                    {
                      const particle *p2 = new particle;
                      set(p2, {(*speed2), (*pos2)});
                    }
                    {
                      const bag **b2 = new bag *;
                      set(b2, array_access(bagsNext, (*idCell2)));
                    }
                    bag_push((*b2), (*p2));
                  }
                  delete b2;
                  delete p2;
                  delete idCell2;
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
