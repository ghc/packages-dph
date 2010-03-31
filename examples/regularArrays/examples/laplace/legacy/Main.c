
// Naive solver for the Laplace equation.
//	Boundary conditions are fixed on the edge of the square.
//
//	This method is very slow to converge for large matrices.
//	If we were going to do it properly we'd start with a matrix a fraction of the size
//	of the final result, solve that, then use those values to tile a larger initial 
//	matrix, solve that etc.. working our way up to the final result. Doing this would
//	help propagate information from the boundary conditions throughout the matrix.
//
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <sys/time.h>
#include <sys/resource.h>

// A matrix represented as an array of rows.
typedef struct {
	int 	 width;
	int 	 height;
	double** data;
} Matrix;


// Matrix Creation and Freeing --------------------------------------------------------------------
// Given a function that produces each element, 
//	create a matrix of a given size.
Matrix* createMatrix
	( int width
	, int height
	, double (*mkElem)(int width, int height, int x, int y))
{
	double** data	= malloc (sizeof(double*) * height);

	for (int y = 0; y < height; y++) {
		data[y]	= malloc (sizeof(double) * width);
		
		for (int x = 0; x < width; x++)
			data[y][x] = mkElem(width, height, x, y);
	}
	
	Matrix* mat	= malloc (sizeof(Matrix));
	mat->width	= width;
	mat->height	= height;
	mat->data	= data;
	return mat;
}


void freeMatrix (Matrix* mat)
{
	for (int y = 0; y < mat->height; y++)
		free(mat->data[y]);
	
	free(mat->data);
	free(mat);
}


// Check whether these matrices have the same width and height.
int matricesHaveSameShape (Matrix* mat1, Matrix* mat2)
{
	return	(mat1->width  == mat2->width)
	    &&  (mat1->height == mat2->height);
}


// Boundary Conditions ----------------------------------------------------------------------------
// Make the mask for the boundary conditions.
//	Should return 0 when the point is part of the boundary, and 1 otherwise.
double mkBoundaryMask (int width, int height, int x, int y)
{
	int w 	= width  - 1;
	int h	= height - 1;
	
	if      (x == 0)			return 0;
	else if (y == 0)			return 0;
	else if (x >= w)			return 0;
	else if (y >= h)			return 0;
	else					return 1;
}


// Make the values for the boundary conditions.
//	Should return 0 where the point is not part of the boundary.
double mkBoundaryValue (int width, int height, int x, int y)
{
	int w 	= width  - 1;
	int h	= height - 1;
	
	if 	(x == 0 && y > 0 && y < h)	return 80;
	else if (y == 0 && x > 0 && x < w)	return 20;
	else if (x == w && y > 0 && y < h)	return 0;
	else if	(y == h && x > 0 && x < w)	return 180;
	else					return 0;
}


// Apply the boundary conditions to this matrix.
//	The mask  matrix has 0 in places where boundary conditions hold
//	and 1 otherwise.
//
//	The value matrix has the boundary condition value in places where it holds,
//	and 0 otherwise.
// 
void applyBoundary
	( Matrix* matDest
	, Matrix* matBoundMask
	, Matrix* matBoundValue)
{
	assert(matricesHaveSameShape(matDest, matBoundMask));
	assert(matricesHaveSameShape(matDest, matBoundValue));

	for (int y = 0; y < matDest->height; y++)
	for (int x = 0; x < matDest->width; x++) {
		matDest->data[y][x]
			= (matDest->data[y][x] * matBoundMask->data[y][x]) 
			+ matBoundValue->data[y][x];
	}
}
		

// Relaxation -------------------------------------------------------------------------------------
// Perform one relaxation cycle with a four point stencil for the Laplace equation.
void relaxLaplace 
	( Matrix* matDest
	, Matrix* matSrc)
{
	assert(matricesHaveSameShape(matDest, matSrc));
	
	for (int y = 1; y < matDest->height - 1; y++) 
	for (int x = 1; x < matDest->width  - 1; x++) {
		double left	= matSrc->data[y]  [x-1];
		double right	= matSrc->data[y]  [x+1];
		double up	= matSrc->data[y+1][x];
		double down	= matSrc->data[y-1][x];
		
		matDest->data[y][x] = (left + right + up + down) / 4;
	}	
}


// Solver -----------------------------------------------------------------------------------------
// Main solver loop.
//	Relax the matrix, then apply boundary conditions, for some number of iterations.
//	The values for the next iteration are written to matDest, 
//	then the matInitial and matDest buffers are swapped. 
//
//	Returns either matInitial or matDest, depending on how many iterations we took.
//
Matrix* solve
	( int iterations
	, Matrix* matBoundMask
	, Matrix* matBoundValue
	, Matrix* matInitial
	, Matrix* matDest)	// Where to write the result of the first iteration.
{
	assert(matricesHaveSameShape(matDest, matInitial));
	assert(matricesHaveSameShape(matDest, matBoundValue));
	assert(matricesHaveSameShape(matDest, matBoundMask));

	Matrix* matTmp	= 0;

	for (int i = 0; i < iterations; i++) {
		relaxLaplace  (matDest, matInitial);
		applyBoundary (matDest, matBoundMask, matBoundValue);

		matTmp		= matDest;
		matDest		= matInitial;
		matInitial	= matTmp;
	}

	// Return result of last iteration.
	return	matTmp;
}	



// Color Ramps ------------------------------------------------------------------------------------
// Standard Hot -> Cold hypsometric color ramp.
//	Sequence is red, yellow, green, cyan, blue.
//	All values are clamped to [vmin .. vmax]
void rampColorHotToCold
	 (double v
	, double vmin		
	, double vmax
	, double* r		// color component outputs
	, double* g
	, double* b)
{
	if (v < vmin)	v = vmin;
	if (v > vmax)	v = vmax;
	double dv = vmax - vmin;

	if (v < (vmin + 0.25 * dv)) {
		*r = 0;
		*g = 4 * (v - vmin) / dv;
		*b = 1;
	} 
	else if (v < (vmin + 0.5 * dv)) {
		*r = 0;
		*g = 1;
		*b = 1 + 4 * (vmin + 0.25 * dv - v) / dv;
	}
	else if (v < (vmin + 0.75 * dv)) {
		*r = 4 * (v - vmin - 0.5 * dv) / dv;
		*g = 1;
		*b = 0;
	} 
	else {
		*r = 1;
		*g = 1 + 4 * (vmin + 0.75 * dv - v) / dv;
		*b = 0;
	}
}
	
	
// PPM --------------------------------------------------------------------------------------------
void writeMatrixAsPPM
	( char*  	fileName
	, Matrix* 	mat )
{
	FILE* file	= fopen(fileName, "w+");
	fprintf(file, "P3\n");
	fprintf(file, "%d %d\n", mat->width, mat->height);
	fprintf(file, "255\n");
	
	for (int y = 0; y < mat->height; y++)
	for (int x = 0; x < mat->width; x++) {
		double v = mat->data[y][x];

		double r = 0;
		double g = 0;
		double b = 0;
		rampColorHotToCold(v, 0, 180, &r, &g, &b);

		fprintf	( file
			, "%d %d %d\n"
			, (int)(r * 255)
			, (int)(g * 255)
			, (int)(b * 255) );		
	}

	fclose(file);
}

// Timing

void
add_timeval( struct timeval *x, const struct timeval *y )
{
  x->tv_sec += y->tv_sec;
  x->tv_usec += y->tv_usec;
  if( x->tv_usec > 1000000 ) {
    ++x->tv_sec;
    x->tv_usec -= 1000000;
  }
}

void
sub_timeval( struct timeval *x, const struct timeval *y )
{
  x->tv_sec -= y->tv_sec;
  if( x->tv_usec < y->tv_usec ) {
    --x->tv_sec;
    x->tv_usec = x->tv_usec + (1000000 - y->tv_usec);
  } else
    x->tv_usec -= y->tv_usec;
}

void
print_timeval( const struct timeval *t )
{
  printf( "%ld", (long int) t->tv_sec * 1000 + (long int) t->tv_usec / 1000 );
}


// Main -------------------------------------------------------------------------------------------
int main(int argc, char** argv)
{
	// Argument parsing
	if (argc != 5) {
		printf("Usage: laplace <width> <height> <iterations> <output file.ppm>\n");
		printf("  width, height  :: Int      The width and height of the matrix\n");
		printf("  iterations     :: Int      Number of iterations to use in the solver\n");
		exit(0);
	}
	int width	= 0;
	int height	= 0;
	int iterations	= 0;
	
	if(sscanf(argv[1], "%d", &width) != 1) {
		printf("laplace: can't parse matrix width\n");
		exit(1);
	}

	if(sscanf(argv[2], "%d", &height) != 1) {
		printf("laplace: can't parse matrix height\n");
		exit(1);
	}

	if(sscanf(argv[3], "%d", &iterations) != 1) {
		printf("laplace: can't parse iterations\n");
		exit(1);
	}
		
	char* fileName	= argv[4];

	
	// Setup boundary condition matrices
	Matrix*	matBoundMask	= createMatrix (width, height, mkBoundaryMask);
	Matrix*	matBoundValue	= createMatrix (width, height, mkBoundaryValue);	
	
	// Set the initial matrix to the same as the boundary conditions.
	Matrix*	matInitial	= createMatrix (width, height, mkBoundaryValue);
	
	// A destination buffer, to write the next iteration into.
	Matrix* matDest		= createMatrix (width, height, mkBoundaryValue);
	
	// Run the solver.
	//	The result is either the matInitial or matBuffer, depending
	//	on how many iterations we took.
        struct timeval start, finish;
        struct rusage start_ru, finish_ru;

        gettimeofday( &start, NULL );
        getrusage( RUSAGE_SELF, &start_ru );

	Matrix* matFinal	
		= solve ( iterations
			, matBoundMask, matBoundValue
			, matInitial, matDest);

        gettimeofday( &finish, NULL );
        getrusage( RUSAGE_SELF, &finish_ru );

	// Write the output to a PPM file.
	writeMatrixAsPPM(fileName, matFinal);
	
	// Cleanup
	freeMatrix (matBoundMask);
	freeMatrix (matBoundValue);
	freeMatrix (matInitial);
	freeMatrix (matDest);

        sub_timeval( &finish, &start );
        sub_timeval( &finish_ru.ru_utime, &start_ru.ru_utime );
        sub_timeval( &finish_ru.ru_stime, &start_ru.ru_stime );
        add_timeval( &finish_ru.ru_utime, &finish_ru.ru_stime );

        print_timeval( &finish ); putchar( '/' );
        print_timeval( &finish_ru.ru_utime); putchar( '\n' );
}

