
// Naive solver for the Laplace equation.
//	Uses a square matrix.
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


// Represent our matrices as arrays of rows.
typedef double** Matrix;


// Boundary Conditions ----------------------------------------------------------------------------
// Make the mask for the boundary conditions.
//	Should return 0 when the point is part of the boundary, and 1 otherwise.
double mkBoundaryMask (int size, int x, int y)
{
	if      (x == 0)			return 0;
	else if (y == 0)			return 0;
	else if (x >= size - 1)			return 0;
	else if (y >= size - 1)			return 0;
	else					return 1;
}


// Make the values for the boundary conditions.
//	Should return 0 where the point is not part of the boundary.
double mkBoundaryValue (int size, int x, int y)
{
	int n = size - 1;
	if 	(x == 0 && y > 0 && y < n)	return 80;
	else if (y == 0 && x > 0 && x < n)	return 20;
	else if (x == n && y > 0 && y < n)	return 0;
	else if	(y == n && x > 0 && x < n)	return 180;
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
	( int size
	, Matrix matDest
	, Matrix matBoundMask
	, Matrix matBoundValue)
{
	for (int y = 0; y < size; y++)
	for (int x = 0; x < size; x++) {
		matDest[y][x]
			= (matDest[y][x] * matBoundMask[y][x]) 
			+ matBoundValue[y][x];
	}
}
		

// Matrix Creation and Freeing --------------------------------------------------------------------
// Given a function that produces each element, 
//	create a matrix of a given size.
Matrix createMatrix
	( int size
	, double (*mkElem)(int size, int x, int y))
{
	Matrix mat	= malloc (sizeof(double*) * size);

	for (int y = 0; y < size; y++) {
		mat[y]	= malloc (sizeof(double) * size);
		
		for (int x = 0; x < size; x++)
			mat[y][x] = mkElem(size, x, y);
	}
	
	return mat;
}

void freeMatrix (int size, Matrix mat)
{
	for (int y = 0; y < size; y++)
		free(mat[y]);
	
	free(mat);
}


// Relaxation -------------------------------------------------------------------------------------
// Perform one relaxation cycle with a four point stencil for the Laplace equation.
void relaxLaplace 
	( int size
	, Matrix matDest
	, Matrix matSrc)
{
	for (int x = 1; x < size - 1; x++)
	for (int y = 1; y < size - 1; y++) {
		double left	= matSrc[y]  [x-1];
		double right	= matSrc[y]  [x+1];
		double up	= matSrc[y+1][x];
		double down	= matSrc[y-1][x];
		
		matDest[y][x]	= (left + right + up + down) / 4;
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
Matrix 	solve
	( int size
	, int iterations
	, Matrix matBoundMask
	, Matrix matBoundValue
	, Matrix matInitial
	, Matrix matDest)	// Where to write the result of the first iteration.
{
	Matrix matTmp	= 0;

	for (int i = 0; i < iterations; i++) {
		relaxLaplace  (size, matDest, matInitial);
		applyBoundary (size, matDest, matBoundMask, matBoundValue);

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
	( char*  fileName
	, int	 size
	, Matrix mat )
{
	FILE* file	= fopen(fileName, "w+");
	fprintf(file, "P3\n");
	fprintf(file, "%d %d\n", size, size);
	fprintf(file, "255\n");
	
	for (int y = 0; y < size; y++)
	for (int x = 0; x < size; x++) {
		double v = mat[y][x];

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



// Main -------------------------------------------------------------------------------------------
int main(int argc, char** argv)
{
	// Argument parsing
	if (argc != 4) {
		printf("Usage: laplace <matrix dim> <iterations> <output file.ppm>\n");
		printf("  matrix dim :: Int      Both the width and height of the matrix\n");
		printf("  iterations :: Int      Number of iterations to use in the solver\n");
		exit(0);
	}
	int size	= 0;
	int iterations	= 0;
	
	if(sscanf(argv[1], "%d", &size) != 1) {
		printf("laplace: can't parse matrix dim\n");
		exit(1);
	}

	if(sscanf(argv[2], "%d", &iterations) != 1) {
		printf("laplace: can't parse iterations\n");
		exit(1);
	}
		
	char* fileName	= argv[3];
	

	// Setup boundary condition matrices
	Matrix	matBoundMask	= createMatrix (size, mkBoundaryMask);
	Matrix	matBoundValue	= createMatrix (size, mkBoundaryValue);	
	
	// Set the initial matrix to the same as the boundary conditions.
	Matrix	matInitial	= createMatrix (size, mkBoundaryValue);
	
	// A destination buffer, to write the next iteration into.
	Matrix 	matDest		= createMatrix (size, mkBoundaryValue);
	
	// Run the solver.
	//	The result is either the matInitial or matBuffer, depending
	//	on how many iterations we took.
	Matrix matFinal	
		= solve ( size, iterations
			, matBoundMask, matBoundValue
			, matInitial, matDest);
	
	// Write the output to a PPM file.
	writeMatrixAsPPM(fileName, size, matFinal);
	
	// Cleanup
	freeMatrix (size, matBoundMask);
	freeMatrix (size, matBoundValue);
	freeMatrix (size, matInitial);
	freeMatrix (size, matDest);
}

