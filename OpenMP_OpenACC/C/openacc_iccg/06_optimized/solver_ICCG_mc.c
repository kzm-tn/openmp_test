/*
 * solver_ICCG
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <math.h>

#include "solver_ICCG_mc.h"

extern int
solve_ICCG_mc(int N, int NL, int NU, int NPL, int NPU, int *indexL, int *itemL, int *indexU, int *itemU,
	      double *D, double *B, double *X, double *AL, double *AU,
	      int NCOLORtot, int PEsmpTOT, int *SMPindex, int *SMPindexG,
	      double EPS, int *ITR, int *IER)
{
        double **W;
        double VAL, BNRM2, WVAL, SW, RHO, BETA, RHO1, C1, DNRM2, ALPHA, ERR;
	int i, j, ic, ip, L, ip1, ip2;
	int R = 0;
	int Z = 1;
	int Q = 1;
	int P = 2;
	int DD = 3;

/*********
 * INIT. *
 *********/

	//	W = (double **)allocate_matrix(sizeof(double), 4, N);
	allocate_matrix2(&W,sizeof(double), 4, N);

#pragma acc enter data                  \
  copyin(D[0:N])                                \
  copyin(X[0:N])                                \
  copyin(indexL[0:N+1])                         \
  copyin(AL[0:NPL])                             \
  copyin(itemL[0:NPL])                          \
  copyin(indexU[0:N+1])                         \
  copyin(AU[0:NPU])                             \
  copyin(itemU[0:NPU])                          \
  copyin(B[0:N])                                \
  copyin(SMPindex[0:NCOLORtot*PEsmpTOT])        \
  create(W[0:4][0:N])
  

#pragma omp parallel for private (i)
#pragma acc kernels present(X,W)
#pragma acc loop independent
	  for(i=0; i<N; i++) {
	    X[i] = 0.0;
	    W[1][i] = 0.0;
	    W[2][i] = 0.0;
	    W[3][i] = 0.0;
	  }
	  
#pragma omp parallel private (ic, ip1, ip2, i, WVAL, j)
	  for(ic=0; ic<NCOLORtot; ic++) {
	    ip1 = ic * PEsmpTOT;
	    ip2 = ic * PEsmpTOT + PEsmpTOT;
#pragma omp for
#pragma acc kernels present(SMPindex,D,indexL,AL,W,itemL,W)
#pragma acc loop independent
	    for(i=SMPindex[ip1]; i<SMPindex[ip2]; i++) {
	      VAL = D[i];
	      for(j=indexL[i]; j<indexL[i+1]; j++) {
		VAL = VAL - AL[j]*AL[j] * W[DD][itemL[j] - 1];
	      }
	      W[DD][i] = 1.0 / VAL;
	    }
	  }
	  
	  /**************************
	   * {r0} = {b} - {A}{xini} *
	   **************************/
#pragma omp parallel for private (i, VAL, j)
#pragma acc kernels present(D,X,indexL,AL,itemL,indexU,AU,itemU,W,B)
#pragma acc loop independent
	  for(i=0; i<N; i++) {
	    VAL = D[i] * X[i];
	    for(j=indexL[i]; j<indexL[i+1]; j++) {
	      VAL += AL[j] * X[itemL[j]-1];
	    }
	    for(j=indexU[i]; j<indexU[i+1]; j++) {
	      VAL += AU[j] * X[itemU[j]-1];
	    }
	    W[R][i] = B[i] - VAL;
	  }
	  
	  BNRM2 = 0.0;
#pragma omp parallel for private (i) reduction (+:BNRM2)
#pragma acc kernels  present(B)
#pragma acc loop reduction (+:BNRM2)
	  for(i=0; i<N; i++) {
	    BNRM2 += B[i]*B[i];
	  }
	  
	  /************************************************************** ITERATION */
	  *ITR = N;
	  
	  for(L=0; L<(*ITR); L++) {
	    
	    /*******************
	     * {z} = [Minv]{r} *
	     *******************/
#pragma omp parallel for private(i)
#pragma acc kernels present(W) async(0)
#pragma acc loop independent
	    for(i=0; i<N; i++) {
	      W[Z][i] = W[R][i];
	    }
	    
#pragma omp parallel private (ic, ip1, ip2, i, WVAL, j)
	    for(ic=0; ic<NCOLORtot; ic++) {
	      ip1 = ic * PEsmpTOT;
	      ip2 = ic * PEsmpTOT + PEsmpTOT;
#pragma omp for
#pragma acc kernels present(SMPindex,indexL,AL,itemL,W) async(0)
#pragma acc loop independent
	      for(i=SMPindex[ip1]; i<SMPindex[ip2]; i++) {
		WVAL = W[Z][i];
		for(j=indexL[i]; j<indexL[i+1]; j++) {
		  WVAL -= AL[j] * W[Z][itemL[j]-1];
		}
		W[Z][i] = WVAL * W[DD][i];
	      }
	    }
	    
#pragma omp parallel private (ic,ip1, ip2, i, SW, j)
	    for(ic=NCOLORtot-1; ic>=0; ic--) {
	      ip1 = ic * PEsmpTOT;
	      ip2 = ic * PEsmpTOT + PEsmpTOT;
#pragma omp for
#pragma acc kernels present(SMPindex,indexU,AU,itemU,W) async(0)
#pragma acc loop independent
	      for(i=SMPindex[ip1]; i<SMPindex[ip2]; i++) {
		SW = 0.0;
		for(j=indexU[i]; j<indexU[i+1]; j++) {
		  SW += AU[j] * W[Z][itemU[j]-1];
		}
		W[Z][i] -= W[DD][i] * SW;
	      }
	    }
	    
	    /****************
	     * RHO = {r}{z} *
	     ****************/
	    RHO = 0.0;
#pragma omp parallel for private (i) reduction (+:RHO)
#pragma acc kernels present(W) async(0)
#pragma acc loop reduction(+:RHO) gang vector(256)
	    for(i=0; i<N; i++) {
	      RHO += W[R][i] * W[Z][i];
	    }
#pragma acc wait(0)
	    
	    /********************************
	     * {p}  = {z} if      ITER=0    *
	     * BETA = RHO / RHO1  otherwise *
	     ********************************/
	    if(L == 0) {
#pragma omp parallel for private (i)
#pragma acc kernels present(W) async(0)
#pragma acc loop independent
	      for(i=0; i<N; i++) {
		W[P][i] = W[Z][i];
	      }
	    } else {
	      BETA = RHO / RHO1;
#pragma omp parallel for private (i)
#pragma acc kernels present(W) async(0)
#pragma acc loop independent
	      for(i=0; i<N; i++) {
		W[P][i] = W[Z][i] + BETA * W[P][i];
	      }
	    }
	    
	    /****************
	     * {q} = [A]{p} *
	     ****************/
#pragma omp parallel for private (i, VAL, j)
#pragma acc kernels present(D,W,indexL,AL,itemL,indexU,AU,itemU)  async(0)
#pragma acc loop independent
	    for(i=0; i<N; i++) {
	      VAL = D[i] * W[P][i];
	      for(j=indexL[i]; j<indexL[i+1]; j++) {
		VAL += AL[j] * W[P][itemL[j]-1];
	      }
	      for(j=indexU[i]; j<indexU[i+1]; j++) {
		VAL += AU[j] * W[P][itemU[j]-1];
	      }
	      W[Q][i] = VAL;
	    }
	    
	    /************************
	     * ALPHA = RHO / {p}{q} *
	     ************************/
	    C1 = 0.0;
#pragma omp parallel for private (i) reduction (+:C1)
#pragma acc kernels present(W)  async(0)
#pragma acc loop reduction(+:C1) gang vector(256)
	    for(i=0; i<N; i++) {
	      C1 += W[P][i] * W[Q][i];
	    }
#pragma acc wait(0)
	    
	    ALPHA = RHO / C1;
	    
	    /***************************
	     * {x} = {x} + ALPHA * {p} *
	     * {r} = {r} - ALPHA * {q} *
	     ***************************/
#pragma omp parallel for private (i)
#pragma acc kernels present(X,W)  async(0)
#pragma acc loop independent
	    for(i=0; i<N; i++) {
	      X[i]    += ALPHA * W[P][i];
	    }
	    
#pragma omp parallel for private (i)
#pragma acc kernels present(W) async(0)
#pragma acc loop independent
	    for(i=0; i<N; i++) {
	      W[R][i] -= ALPHA * W[Q][i];
	    }
	    
	    DNRM2 = 0.0;
#pragma omp parallel for private (i) reduction (+:DNRM2)
#pragma acc kernels present(W) async(0)
#pragma acc loop reduction(+:DNRM2) gang vector(256)
	    for(i=0; i<N; i++) {
	      DNRM2 += W[R][i]*W[R][i];
	    }
#pragma acc wait(0)
	    
	    ERR = sqrt(DNRM2/BNRM2);
	    if( (L+1)%100 ==1) {
	      fprintf(stderr, "%5d%16.6e\n", L+1, ERR);
	    }
	    
	    if(ERR < EPS) {
	      *IER = 0;
	      goto N900;
	    } else {
	      RHO1 = RHO;
	    }
	  }
	  *IER = 1;
	N900:

#pragma acc exit data                           \
  delete(D[0:N])                                \
  copyout(X[0:N])                               \
  delete(indexL[0:N+1])                         \
  delete(AL[0:NPL])                             \
  delete(itemL[0:NPL])                          \
  delete(indexU[0:N+1])                         \
  delete(AU[0:NPU])                             \
  delete(itemU[0:NPU])                          \
  delete(B[0:N])                                \
  delete(SMPindex[0:NCOLORtot*PEsmpTOT])        \
  delete(W[0:4][0:N])

	fprintf(stderr, "%5d%16.6e\n", L+1, ERR);
	*ITR = L;
	
	free(W);
	
	return 0;
}
