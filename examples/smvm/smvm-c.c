#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <time.h>

#include <HsFFI.h>

int rows;
int cols;

typedef struct {
  HsInt  size;
  void *data;
} Array;

Array vector;
Array lengths;
Array indices;
Array values;
Array result;

#define DATA(arr,i,t) (((t *)(arr).data)[i])

void new( HsInt size, Array * arr, int el_size )
{
  arr->size = size;
  arr->data = malloc( el_size * size );
}

void load( int file, Array * arr, int el_size )
{
  read( file, &(arr->size), sizeof(HsInt) );
  arr->data = malloc( el_size * arr->size );
  read( file, arr->data, arr->size*el_size );
}

void compute()
{
  HsInt row, el, idx;
  HsDouble sum;

  el = 0;
  idx = 0;
  for( row = 0; row < lengths.size; ++row ) {
    sum = 0;
    for( el = 0; el < DATA(lengths,row,HsInt); ++el ) {
       sum += DATA(values, idx, HsDouble)
            * DATA(vector, DATA(indices, idx, HsInt), HsDouble);
       ++idx;
    }
    DATA(result, row, HsDouble) = sum;
  }
}

HsDouble checksum( Array * arr )
{
  HsDouble sum = 0;
  int i;

  for( i = 0; i < arr->size; ++i )
     sum += DATA((*arr), i, HsDouble);
  return sum;
}
                       
int main( int argc, char * argv[] )
{
  int file, runs;
  clock_t start, finish;

  runs = atoi( argv[1] );	// FIXME: runs are ignored
  file = open( argv[2], O_RDONLY );
  load( file, &lengths, sizeof(HsInt) );
  load( file, &indices, sizeof(HsInt) );
  load( file, &values,  sizeof(HsDouble) );
  load( file, &vector,  sizeof(HsDouble) );
  close(file);
  new( lengths.size, &result, sizeof(HsDouble) );

  printf( "rows = %ld; colums = %ld; elements = %ld\n", (long)lengths.size
                                                      , (long)vector.size
                                                      , (long)values.size );
  start = clock();
  compute(); 
  finish = clock();

  printf( "%ld %Lf\n", (long int)((finish-start) / (CLOCKS_PER_SEC/1000)),
                          (long double)(checksum(&result)) );

  return 0;
}

