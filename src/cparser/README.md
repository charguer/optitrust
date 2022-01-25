
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
```
