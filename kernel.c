/* 
 * This code needs to be modified/extended
 * modification:
 * - 
 * extension:
 * - more kernel_codes
 *
 * Pouria Fewzee, 2013
 *
 * input variables:
 * - type: self vs mutual kernel
 *   - self: 0
 *   - mutual: 1
 * - dim: vector of two entries: 
 *   - [0]: number of instances
 *   - [1]: dimensionality of feature space
 * - X: data matrix, reshaped as a vector by rows
 *   for the case of mutual kernel, the two matrices
 *   are assumed to be concatenated
 * - kernel_code: type of the kernel
 *   - lin: 0
 *   - rbf: 1
 *   - pol: 2
 * - param: kernel-specific parameters
 *   - lin: not used
 *   - rbf: exp(-param[0]*x)
 *   - pol: pow(param[2]*x + param[1], param[0])
 * - K: kernel matrix, reshaped as a vector by rows
 */

#include <math.h>
#include <stdio.h>

/* Auxiliary routines' prototypes */
double map(double *X, int i, int j, int k, int p, int kernel_code);
void reduce(double sum, int kernel_code, double *param, double *K,
            int i, int j, int n);

double lin(double x, double *param);
double rbf(double x, double *param);
double pol(double x, double *param);

void kernel(int *type, int *dim, double *X, int *kernel_code,
            double *param, double *K)
{
    // 0 for self kernel - 1 for mutual kernel
    int t = type[0];
    // number of instances
    int n1 = dim[0];
    // dimensionaity of feature space
    int p = dim[1];
    // number of instances in the second matrix
    int n2 = 0;
    if (t == 1) n2 = dim[2];

    // supported kernel functions
    double (*reduce[])() = {lin, rbf, pol};

    int i, j, k;
    int init = n1;
    double sum;
    double tmp;

    // going over instances of the first matrix
    for (i = 0; i < n1; i++) {
        if (t == 0) init = i;
        // going over instances of the second matrix
        for (j = init; j < n1+n2; j++) {
            sum = 0;
            // going over features
            for (k = 0; k < p; k++) {
                sum += map(X, i, j, k, p, kernel_code[0]);
            }
            tmp = reduce[kernel_code[0]](sum, param);
            if (t == 0) {
                K[i*n1+j] = tmp;
                K[j*n1+i] = tmp;
            } else {
                K[i*n2+j-n1] = tmp;
            }
        }
    }
}

double map(double *X, int i, int j, int k, int p, int kernel_code)
{
    double tmp;

    if (kernel_code == 0 || kernel_code == 2) {
        return X[i*p+k] * X[j*p+k];
    } else if (kernel_code == 1) {
        tmp = X[i*p+k] - X[j*p+k];
        return tmp*tmp;
    }
}

double lin(double x, double *param)
{
    return x;
}

double rbf(double x, double *param)
{
    return exp(-param[0]*x);
}

double pol(double x, double *param)
{
    return pow(param[2]*x + param[1], param[0]);
}
