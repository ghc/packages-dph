#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>

#include <HsFFI.h>

#define cols 100
#define rows 100
#define ratio 0.1

HsInt lengths[rows];
HsInt indices[cols];

double gen_doubles( int file, HsInt n )
{
  HsDouble d;
  HsDouble sum = 0;

  int a, b;

  while( n != 0 )
  {
    a = random() % 1000;
    b = random() % 1000;
    if (a == 0 || b == 0)
      d = 0.1;
    else
      d = ((HsDouble)a) / ((HsDouble)b);

    write( file, &d, sizeof(HsDouble) );
    sum += d;
    --n;
  }
  return sum;
}

HsInt gen_lengths()
{
  HsInt i;
  HsInt n = 0;

  int range = ((double)cols * 2) * ratio;
  
  for( i = 0; i < rows; ++i ) {
    lengths[i] = random() % range;
    n += lengths[i];
  }

  return n;
}

int find_index( int from, int to, HsInt idx )
{
  while( from != to ) {
    if( indices[from] == idx ) return 1;
    ++from;
  }
  return 0;
}

int cmp_HsInt( const void *p, const void *q )
{
  HsInt x = *(HsInt *)p;
  HsInt y = *(HsInt *)q;

  if( x < y ) return -1;
  if( x > y ) return 1;
  return 0;
}

void gen_indices( int file )
{
  HsInt i, j, k;

  k = 0;
  for( i = 0; i < rows; ++i ) {
    for( j = 0; j < lengths[i]; ++j ) {
      do {
        indices[j] = random() % cols;
      } while( find_index( 0, j, indices[j] ) );
    }
    qsort( indices, j, sizeof(HsInt), cmp_HsInt );
    write( file, indices, sizeof(HsInt) * j );
  }
}

int main( int argc, char *argv[] )
{
  HsInt k, n;
  int file;

  HsDouble sum1,sum2;

  file = open( argv[1], O_CREAT | O_WRONLY | O_TRUNC );

  k = rows;
  n = gen_lengths();
  write( file, &k, sizeof(k) );
  write( file, lengths, sizeof(HsInt) * rows );
  write( file, &n, sizeof(n) );
  gen_indices( file );
  write( file, &n, sizeof(n) );
  sum1 = gen_doubles(file, n);
  k = cols;
  write( file, &k, sizeof(k) );
  sum2 = gen_doubles(file, cols);
  close(file);

  printf( "%Lf %Lf\n", (long double)sum1, (long double)sum2 );
  return 0;
}

