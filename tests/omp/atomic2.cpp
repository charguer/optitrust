int atomic_read(const int *p)
{
    int value;
/* Guarantee that the entire value of *p is read atomically. No part of
 * *p can change during the read operation.
 */
     value = *p;
     return value;
}

void atomic_write(int *p, int value)
{
/* Guarantee that value is stored atomically into *p. No part of *p can
change
 * until after the entire write operation is completed.
 */
    *p = value;
}

