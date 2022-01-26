
DO NOT EDIT THE FILES IN THIS DIRECTORY.

They are imported from a patched version of CompCert
https://github.com/charguer/CompCert

to support a few language extensions:

```
int t[avariable];
  meaning that array size are not restricted to plain integers

return { 1, 2 };
  meaning that literals can appear as argument of return

int& x = y;
  meaning that reference types are supported for local variables

typedef struct { int x; int y; } vect;
   meaning that the name of the struct may be given only once
```



TOFIX


int main(void)
{
  {
    int x = 3;
    int y = f(x);
    int z = g(x);
    int u = h(x);
    int * q = malloc(sizeof(int));
    *q = 3;
    m(q);
    int result = k(result, 4);
    return 0;
  }
  return 0;
}

  {
    int idCell = 0;
    for (/*nothing*/; idCell < nbCells; idCell++) {
      bag_swap(&bagsCur[idCell], &bagsNext[idCell]);
    }
  }


struct vect;

struct vect {
  double x;
  double y;
  double z;
};

typedef struct  vect;



typedef struct vect {
  double x;
  double y;
  double z;
} vect, vec2;
