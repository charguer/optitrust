 union {int n; float x;} u;

void atomic_wrong ()
{

  {
    u.n++;

    u.x += 1.0;

/* Incorrect because the atomic constructs reference the same location
   through incompatible types */
  }
}

