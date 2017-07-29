#include <stdio.h>
// For the CUDA runtime routines (prefixed with "cuda_")
#include <cuda_runtime.h>

#define TIMING_SUPPORT

#ifdef TIMING_SUPPORT
#include <helper_cuda.h>
#include <helper_functions.h>
#endif

#define CUDA_TIMING

/**
    CUDA Kernel Device code
    -simple parallel code
    -no optimisation
*/
__global__ void
p_vectorAverageSimple(const float *A, float *B, int numElements, int n) {
    int i = blockDim.x * blockIdx.x + threadIdx.x;
    int counter = 0;
    B[i] = 0;

    if (i < numElements) {
    	for(int j = 0; j < n; j++) {
    		int index = (i-n+1+j);
            if(index >= 0) {
    		    B[i] += A[max(0,index)];
                counter++;
            }
    	}
        B[i] = B[i]/(float)counter;
    }
}

#define BLOCK_SIZE 1024

__global__ void
p_scan(float *X, float *Y, int len) {
	__shared__ float XY[BLOCK_SIZE*2]; // 2 buffers
	int rBuf = 0, wBuf = BLOCK_SIZE;
	int i = blockIdx.x * blockDim.x + threadIdx.x;
	if(i < len) {
		XY[wBuf + threadIdx.x] = X[i];
	}

	for(uint s = 1; s < BLOCK_SIZE; s*= 2) {
		__syncthreads();
		wBuf = BLOCK_SIZE - wBuf;
		rBuf = BLOCK_SIZE - rBuf;
		if(threadIdx.x >= s) {
			XY[wBuf+threadIdx.x] = XY[rBuf+threadIdx.x - s] + XY[rBuf+threadIdx.x];
		}
		else {
			// If not adding, thread should copy
			XY[wBuf+threadIdx.x] = XY[rBuf+threadIdx.x];
		}
	}
	if(i < len) {
		Y[i] = XY[wBuf + threadIdx.x];
	}
}

__global__ void
p_vectorAverageInefficient(const float *A, float *B, int numElements, int n) {
    int i = blockDim.x * blockIdx.x + threadIdx.x;
	if(i >= n) {
		int index = i-n;
		B[i] = A[i]-A[index];
		B[i] = B[i]/n;
	}
	else {
		B[i] = A[i]/(float)(i+1);
	}
}



/**
    Device code for average
*/
void
s_vectorAverageSimple(const float *A, float *B, int numElements, int n) {
    for(int i = 0; i < numElements; i++) {
    	int counter = 0;
        B[i] = 0;
    	for(int j = 0; j < n; j++) {
    		int index = (i-n+1+j);
    		if(index >= 0) {
                B[i] += A[max(0,index)];
    			counter++;
    		}
    	}
    	B[i] = B[i]/(float)counter;
    }
}

/**
	Device code for scan
 */
void
s_scan(const float *x, float *y, int len) {
	y[0] = x[0];
	for(int i = 0; i < len; i++) {
		y[i] = y[i-1] + x[i];
	}
}

void
s_vectorAverageEfficient(const float *A, float *B, float *C, int numElements, int n) {
	s_scan(A, C, numElements);
	for(int i = 0; i < numElements; i++) {
		if(i >= n) {
			int index = i-n;
			B[i] = C[i]-C[index];
			B[i] = B[i]/n;
		}
		else {
			B[i] = C[i]/(float)(i+1);
		}
	}
}



/**
    Host main routine
*/
int
main(void) {
    // Error code check for CUDA calls
    cudaError_t err = cudaSuccess;

    // Print the vector length to be used and compute size
    int numElements = 10;
    int windowSize = 5;
    size_t size = numElements * sizeof(float);
    printf("[Vector average of %d elements]\n", numElements);

    // Allocate host input vector A
    float *h_A = (float *)malloc(size);
    // Allocate host input vector B
    float *h_B = (float *)malloc(size);
    // Allocate host input vector C - used for serial version to check parallel
    float *h_C = (float *)malloc(size);
    // Allocate host input vector D - used for serial version to check parallel
    float *h_D = (float *)malloc(size);

    // Verify success of allocations
    if(h_A == NULL || h_B == NULL || h_C == NULL || h_D == NULL) {
        fprintf(stderr, "Failed to allocate host vectors!\n");
        exit(EXIT_FAILURE);
    } 

    // Initialise host vectors
    for(int i = 0; i < numElements; i++) {
        h_A[i] = rand()/(float)RAND_MAX;
    }

    // Allocate the device inp ut vector A
    float *d_A = NULL;
    err = cudaMalloc((void **)&d_A, size);

    if (err != cudaSuccess) {
		fprintf(stderr, "Failed to allocate device vector A (error code %s)!\n", cudaGetErrorString(err));
		exit(EXIT_FAILURE);
	}
    // Allocate the device input vector B
    float *d_B = NULL;
    err = cudaMalloc((void **)&d_B, size);

    if (err != cudaSuccess) {
		fprintf(stderr, "Failed to allocate device vector B (error code %s)!\n", cudaGetErrorString(err));
		exit(EXIT_FAILURE);
	}

    // Copy the host input vector A in host memory to the device input vectors in
	// device memory
	printf("Copy input data from the host memory to the CUDA device\n");
	err = cudaMemcpy(d_A, h_A, size, cudaMemcpyHostToDevice);

	if (err != cudaSuccess) {
		fprintf(stderr, "Failed to copy vector A from host to device (error code %s)!\n", cudaGetErrorString(err));
		exit(EXIT_FAILURE);
	}


	// Launch the Vector Average CUDA Kernel
	int nIter = 1; // Number of iterations to run the kernel
	int threadsPerBlock = 1024;
    // Note this pattern, based on integer division, for rounding up
    int blocksPerGrid = 1 + ((numElements - 1) / threadsPerBlock);

    printf("%d iterated launches of the CUDA kernel with %d blocks of %d threads\n",
        		nIter, blocksPerGrid, threadsPerBlock);

#ifdef TIMING_SUPPORT
    StopWatchInterface *timer = NULL;
    sdkCreateTimer(&timer);             // create a timer
    sdkStartTimer(&timer);               // start the timer
#endif
#ifdef CUDA_TIMING
    cudaEvent_t start, stop;
    float time;

    cudaEventCreate(&start);
    cudaEventCreate(&stop);

    cudaEventRecord( start, 0 );
#endif

    // Call serial version
    //s_vectorAverageSimple(h_A, h_C, numElements, windowSize);
    s_vectorAverageEfficient(h_A, h_C, h_D, numElements, windowSize);
    //s_scan(h_A, h_C, numElements);

    //for (int j = 0; j < nIter; j++)

        // Call kernel version
        //p_vectorAverageSimple<<<blocksPerGrid, threadsPerBlock>>>(d_A, d_B, numElements, windowSize);
    	p_scan<<<blocksPerGrid, threadsPerBlock>>>(d_A, d_B, numElements);
    	p_vectorAverageInefficient<<<blocksPerGrid, threadsPerBlock>>>(d_A, d_B, numElements, windowSize);

#ifdef CUDA_TIMING
    cudaEventRecord( stop, 0 );
    cudaEventSynchronize( stop );

    err = cudaEventElapsedTime( &time, start, stop );
    if (err != cudaSuccess) {
        fprintf(stderr, "Failed to get elapsed time (error code %s)!\n", cudaGetErrorString(err));
        exit(EXIT_FAILURE);
    }

    cudaEventDestroy( start );
    cudaEventDestroy( stop );
    printf("CUDA_TIMING: %.4f ms\n", time);
#endif

    // wait for device to finish
    cudaDeviceSynchronize();
    err = cudaGetLastError();

    if (err != cudaSuccess) {
        fprintf(stderr, "Failed to launch vectorAdd kernel (error code %s)!\n", cudaGetErrorString(err));
        exit(EXIT_FAILURE);
    }


#ifdef TIMING_SUPPORT
    // stop and destroy timer
    sdkStopTimer(&timer);
    double dSeconds = sdkGetTimerValue(&timer)/(1000.0);
    double dNumOps = 1.0e-9 * nIter * size;
    double gflops = dNumOps/dSeconds;

    //Log throughput, etc

    printf("Throughput = %.4f GFlops\nTime = %.5f s\nSize = %.5f Gops\n\n",
  	   gflops, dSeconds, dNumOps);
    sdkDeleteTimer(&timer);
#endif


// Copy the device result vector in device memory to the host result vector
    // in host memory.
    printf("Copy output data from the CUDA device to the host memory\n");
    err = cudaMemcpy(h_B, d_B, size, cudaMemcpyDeviceToHost);

    if (err != cudaSuccess) {
        fprintf(stderr, "Failed to copy vector B from device to host (error code %s)!\n", cudaGetErrorString(err));
        exit(EXIT_FAILURE);
    }


    // Print result
    for(int i = 0; i < numElements; i++) {
        printf("%lf\n", h_C[i]);
    }

	printf("\n");

    for(int i = 0; i < numElements; i++) {
        printf("%lf\n", h_B[i]);
    }


    /*
    // Verify that the result vector is correct
    for (int i = 0; i < numElements; ++i) {
        // fabs function gets absolute value
        if (fabs(h_B[i] - h_C[i]) > 1e-2) {
        	printf("%lf\n%lf", h_B[i], h_C[i]);
            fprintf(stderr, "Result verification failed at element %d!\n", i);
            exit(EXIT_FAILURE);
        }
    }
    printf("Test PASSED\n");
    */



    // Free device global memory
    err = cudaFree(d_A);

    if (err != cudaSuccess) {
        fprintf(stderr, "Failed to free device vector A (error code %s)!\n", cudaGetErrorString(err));
        exit(EXIT_FAILURE);
    }
    err = cudaFree(d_B);

    if (err != cudaSuccess) {
        fprintf(stderr, "Failed to free device vector B (error code %s)!\n", cudaGetErrorString(err));
        exit(EXIT_FAILURE);
    }

    // Free host memory
    free(h_A);
    free(h_B);
    free(h_C);
    free(h_D);

    // Reset the device and exit
    err = cudaDeviceReset();

    if (err != cudaSuccess) {
        fprintf(stderr, "Failed to reset the device! error=%s\n", cudaGetErrorString(err));
        exit(EXIT_FAILURE);
    }
    printf("Done\n");
    return 0;
}
