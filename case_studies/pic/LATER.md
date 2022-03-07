// TODO: it would be nicer if the Poisson module could take rho directly as an array indexed by idCell


// LATER: When ClangML supports it, we'll use overloaded + and * operators on class vect
// LATER: When ClangML supports it, we'll use higher-order iteration with a local function
// LATER: When ClangML supports it, we'll use boost arrays for fixed size arrays


// TODO: rename "_nbCorners" to 8
// TODO: move particle p2 = just before the bag_push
// TODO: replace double cX = 1. + -1. * rX;   with   double cX = 1. - rX;   and use a transformation for this change
// TODO: int x =     make those uppercase in indicesOfCorners



//------------------------------------
// DEPRECATED


#ifdef DEBUG_ACCEL
        if (p->id == 0) {
          printf("particle %d: topcorner_fieldx = %g\n", p->id, field_at_corners.v[0].x);
          printf("particle %d: fieldx = %g\n", p->id, fieldAtPos.x);
          double delta_vx = vect_mul(negHalfStepDuration, accel).x;
          printf("particle %d: delta_vx = %g\n", p->id, delta_vx);
          printf("particle %d: oldvx = %g\n", p->id, p->speed.x);
        }
#endif
#ifdef DEBUG_ACCEL
        if (p->id == 0) {
           printf("particle %d: newvx = %g\n", p->id, p->speed.x);
        }
#endif
#ifdef DEBUG_CREATION_RANDOM
            double rx = pic_vert_next_random_double();
            x = x_range * rx;
            // printf("id = %d, rand = %lf, x = %lf\n", idParticle, rx, x);
#else
            x = x_range * pic_vert_next_random_double();
#endif
+
#ifdef DEBUG_CREATION_RANDOM
            double rx = pic_vert_next_random_double();
            x = x_range * rx + mesh.x_min;
            // printf("id = %d, rand = %lf, x = %lf\n", (int) j, rx, x);
#else
            x = x_range * pic_vert_next_random_double() + mesh.x_min;
#endif


//------------------------------------

/* Other possible implementations for wrap
  // assuming that a particle does not traverse the grid more than once in a timestep
   return (x % gridSize + gridSize) % gridSize
  // use of fmod possible
  // version without modulo but using if statements
    if (x < 0)
       return x + gridSize;
    if (x >= gridSize)
       return x - gridSize;
    return x;
*/


/* DEPRECATED
double_nbCorners vect8_mul(const double a, const double_nbCorners data) {
  double_nbCorners res;
  for (int k = 0; k < nbCorners; k++) {
    res.v[k] = a * data.v[k];
  }
  return res;
}
*/

+
  double s = 0.;
        s += rho[i][j][k];
#ifdef DEBUG_CHARGE
  printf("total charge = %g\n", s);
#endif
