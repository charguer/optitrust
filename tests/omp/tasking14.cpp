void bar(void);

void foo ( )
{
   int i;
   {
      for (i = 0; i < 3; i++) {
          bar();
      }
   }
   {
      for (i = 0; i < 3; i++) {
          bar();
      }
   }
}