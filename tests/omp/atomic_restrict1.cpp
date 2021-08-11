void atomic_wrong ()
{
 union {int n; float x;} u;

  {
    u.n++;

    u.x += 1.0;

/* Incorrect because the atomic constructs reference the same location
   through incompatible types */
  }
}

