#include <stdio.h>
#include <stdlib.h>
#include <vecLib/cblas.h>

void
mult(int *l, int *m, int *n, double *A, double *B, double *C)
{
    double alpha = 1.0, beta = 0.0;

    cblas_dgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, 
                l[0], n[0], m[0], alpha, A, m[0], B, n[0], beta, C, n[0]);
}
