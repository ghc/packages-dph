// gcc -c -O2 matvecmul.c

#include <malloc.h>

float cmvm (float *m, float *v, int n)
{
  int   i, j;
  float *result, sum;

  result = (float*) malloc (n * sizeof (float));
  for (i = 0; i < n; i++) {
    sum = 0;
    for (j = 0; j < n; j++)
      sum += m[i * n + j] * v[j];
    result[i] = sum;
  }
  
  sum = 0;
  for (i = 0; i < n; i++)
    sum += result[i];

  return sum;
}
