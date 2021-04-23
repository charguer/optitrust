int main() {
  {
    const int[10] *t = new int[10];
    set(t, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0});
  }
  {
    {
      { const int[10] *x_split = new int[10]; }
      { const int[10] *z_split = new int[10]; }
      {
        {
          for ({
                 const int *i = new int;
                 set(i, 0);
               };
               ((*i) < 10); operator++(i)) {
            {
              {
                const int *x = new int;
                set(x, ((*i) % 2));
              }
              {
                const int *y = new int;
                set(y, ((*i) / 2));
              }
              {
                const int *z = new int;
                set(z, ((*i) * 3));
              }
              set(array_access(t, (*i)),
                  ((*array_access(t, (*i))) +
                   ((((*i) * (1 - (*x))) + (*y)) - (*z))));
              set(x, (1 - (*x)));
              set(array_access(x_split, (*i)), (*x));
              set(array_access(z_split, (*i)), (*z));
            }
            delete z;
            delete y;
            delete x;
          }
          delete i;
        }
        {
          for ({
                 const int *i = new int;
                 set(i, 0);
               };
               ((*i) < 10); operator++(i)) {
            {
              {
                const int *x = new int;
                set(x, (*array_access(x_split, (*i))));
              }
              {
                const int *z = new int;
                set(z, (*array_access(z_split, (*i))));
              }
              set(array_access(t, (9 - (*i))), ((*array_access(t, (9 - (*i)))) +
                                                (((*i) * (1 - (*x))) - (*z))));
            }
            delete z;
            delete x;
          }
          delete i;
        }
      }
    }
    delete z_split;
    delete x_split;
  }
return_instr : {
  delete t;
  return 0;
}
}
