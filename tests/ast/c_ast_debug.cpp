struct s { int label; union { int i; float f; };};

bool check_struct (struct s b)
{
    // b.label  = 10;
    // b.i = 10;
    // b.f = 10;
    return false;
    // etc.
}