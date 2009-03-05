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
  HsInt result, n;
  clock_t start, finish;

  n = atoi( argv[1] );
  printf( "n = %ld \n", (long)n );

  start = clock();
  result = compute( n ); 
  finish = clock();

  printf( "time = %ld; value = %ld\n", 
	  (long int)((finish-start) / (CLOCKS_PER_SEC/1000)),
	  (long)result);
}

