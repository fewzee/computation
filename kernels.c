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
 * - dim: vector of two entries: 
 *   - [0]: number of instances
 *   - [1]: dimensionality of feature space
 * - X: data matrix, represented as a vector
 * - kernel_code: type of the kernel
 *   - lin: 0
 *   - rbf: 1
 *   - pol: 2
 * - param: kernel-specific parameters
 *   - lin: not used
 *   - rbf: exp(-param[0]*x)
 *   - pol: pow(param[2]*x + param[1], param[0])
 * - K: kernel matrix, represented as a vector
 */

#include <math.h>
#include <stdio.h>

/* Auxiliary routines prototypes */
double map(double *X, int i, int j, int k, int p, int kernel_code);
void reduce(double sum, int kernel_code, double *param, double *K, int i, int j, int n);

double lin(double x, double *param);
double rbf(double x, double *param);
double pol(double x, double *param);

void kernel(int *dim, double *X, int *kernel_code, double *param, double *K)
{
    // number of instances
    int n = dim[0];
    // dimensionaity of feature space
    int p = dim[1];
    // supported kernel function
    double (*reduce[])() = {lin, rbf, pol};

    int i, j, k;
    double sum;

    // going over instances
    for(i = 0; i < n; i++){
        // going over instances
        for(j = i; j < n; j++){
            sum = 0;
            // going over features
            for(k = 0; k < p; k++){
                sum += map(X, i, j, k, p, kernel_code[0]);
            }
            K[i*n+j] = reduce[kernel_code[0]](sum, param);
            K[j*n+i] = K[i*n+j];
        }
    }
}

double map(double *X, int i, int j, int k, int p, int kernel_code)
{
    double temp;

    if      (kernel_code == 0 || kernel_code == 2){
        return X[i*p+k] * X[j*p+k];
    } else if (kernel_code == 1){
        temp = X[i*p+k] - X[j*p+k];
        return temp*temp;
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
