#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <time.h>

#include <HsFFI.h>
#include "Timing.h"

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

int usage()
{
  printf("usage: smvm FILE\n");
  exit(0);
}

                       
int main( int argc, char * argv[] )
{
  int file, runs;

  if (argc != 2) usage();

  char* fileName = argv[1];
  file  = open( fileName, O_RDONLY );
  if(file == -1) {
    printf ("can't open file '%s'\n", fileName);
    exit(1);
  }

  // Check for magic numbers at start of file.
  HsInt magic1, magic2;
  read( file, &magic1,  sizeof(HsInt) );
  read( file, &magic2,  sizeof(HsInt) );
  if (! (magic1 == 0xc0ffee00 && magic2 == 0x12345678)) {
    printf ("bad magic in %s\n",  fileName);
    printf ("got = %0lx, %0lx\n", magic1, magic2);
    exit(1);
  }

  load( file, &lengths, sizeof(HsInt) );
  load( file, &indices, sizeof(HsInt) );
  load( file, &values,  sizeof(HsDouble) );
  load( file, &vector,  sizeof(HsDouble) );
  close(file);
  new( lengths.size, &result, sizeof(HsDouble) );

  // Timing setup
  struct timeval start, finish;
  struct rusage start_ru, finish_ru;

  gettimeofday( &start, NULL );
  getrusage( RUSAGE_SELF, &start_ru );

  // Do the dead
  compute();

  // Print how long it took.
  gettimeofday( &finish, NULL );
  getrusage( RUSAGE_SELF, &finish_ru );


  sub_timeval( &finish, &start );
  sub_timeval( &finish_ru.ru_utime, &start_ru.ru_utime );
  sub_timeval( &finish_ru.ru_stime, &start_ru.ru_stime );
  add_timeval( &finish_ru.ru_utime, &finish_ru.ru_stime );

  printf("elapsedTimeMS   = ");
  print_timeval( &finish ); putchar( '\n' );

  printf("cpuTimeMS       = ");
  print_timeval( &finish_ru.ru_utime); putchar( '\n' );

  printf("result sum      = %Lf\n", (long double)(checksum(&result)));

  printf( "rows = %ld; colums = %ld; elements = %ld\n"
	, (long)lengths.size
        , (long)vector.size
        , (long)values.size );

  return 0;
}

