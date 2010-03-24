#include <stdlib.h>
#include <stdio.h>

typedef double** Matrix;


// Initial Value ----------------------------------------------------------------------------------
double mkInitialValue (int size, int x, int y)
{
	return 0;
}


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
void   applyBoundary
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
		

// Create Matrix ----------------------------------------------------------------------------------
// Given a function that produces each element, 
//	create a matrix of a given size.
Matrix createMatrix
		( int size
		, double (*mkElem)(int size, int x, int y))
{
	Matrix mat	= (Matrix)malloc (sizeof(double*) * size);

	for (int y = 0; y < size; y++) {
		mat[y]	= malloc (sizeof(double) * size);
		
		for (int x = 0; x < size; x++)
			mat[y][x] = mkElem(size, x, y);
	}
	
	return mat;
}


// Relaxation -------------------------------------------------------------------------------------
void relaxLaplace 
	(int size, Matrix matDest, Matrix matSrc)
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
void solve
	( int size
	, int steps
	, Matrix matBoundMask
	, Matrix matBoundValue
	, Matrix matDest)
{
	for (int i = 0; i < steps; i++) {
		relaxLaplace  (size, matDest, matDest);
		applyBoundary (size, matDest, matBoundMask, matBoundValue);
	}
}	


// Color Ramps ------------------------------------------------------------------------------------
void rampColorHotToCold
	 (double v
	, double vmin
	, double vmax
	, double* r
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
	( char* fileName
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
	int size	= 100;
	int steps	= 1000;
	
	Matrix	matBoundMask	= createMatrix (size, mkBoundaryMask);
	Matrix	matBoundValue	= createMatrix (size, mkBoundaryValue);	
	
	Matrix	matDest		= createMatrix (size, mkBoundaryValue);
	
	solve (size, steps, matBoundMask, matBoundValue, matDest);
	
	writeMatrixAsPPM("out.ppm", size, matDest);
}


