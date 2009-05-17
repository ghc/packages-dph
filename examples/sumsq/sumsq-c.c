#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <time.h>

#include <HsFFI.h>

HsInt compute(HsInt n)
{
  HsInt sum = 0;

  while ( n >= 1 ) {
    sum += n * n;
    n--;
  }
  return sum;
}

int main( int argc, char * argv[] )
{
  HsInt result, n, r, i;
  clock_t start, finish, acc;

  n = atoi( argv[2] );
  r = atoi( argv[1] );
  printf( "n = %ld; r = %ld \n", (long)n, (long)r );

  acc = 0;
  for (i = 0; i < r; i++) {
    start = clock();
    result = compute( n ); 
    finish = clock();
    acc += finish - start;
  }

  printf( "time = %ld; value = %ld\n", 
	  (long int)((acc/r) / (CLOCKS_PER_SEC/1000)),
	  (long)result);
  return 0;
}

