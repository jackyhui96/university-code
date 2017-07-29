/*
    - Jacky Hui, 14435743
    - Goals Achieved:
        [x] scan for small arrays
        [x] scan for large arrays
        [x] windowed average calculation from scaned output vector
    - Initial speed up: 6.97
    - Improved speed up: 9.38
        - Tried different configurations of block sizes, initially was 1024, found that 256 is best
*/

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
*/
#define BLOCK_SIZE 256

__global__ void
p_scanEfficient(int *X, int *Y, int len) {
	__shared__ int XY[BLOCK_SIZE];

	int i = blockIdx.x * blockDim.x + threadIdx.x;
	if(i < len) {
		XY[threadIdx.x] = X[i];
	}
	// Reduction phase
	for(uint stride = 1; stride < blockDim.x; stride *= 2)
	{
		__syncthreads();
		uint index = (threadIdx.x + 1) * stride * 2 - 1;
		if(index < blockDim.x) 
			XY[index] += XY[index - stride];
	}

	// Distribution phase
	for(uint stride = BLOCK_SIZE/4; stride > 0; stride /= 2)
	{
		__syncthreads();
		uint index = (threadIdx.x + 1) * stride * 2 - 1;
		if(index + stride < BLOCK_SIZE)
			XY[index + stride] += XY[index];
	}

	__syncthreads();

	if(i < len)
		Y[i] = XY[threadIdx.x];
}

__global__ void
p_vectorAverage(const int *A, float *B, int numElements, int n) {
    int i = blockDim.x * blockIdx.x + threadIdx.x;
	if(i >= n) {
		int index = i-n+1;
		B[i] = (float)(A[i]-A[index])/(float)n;
	}
	else {
		B[i] = (float)A[i]/(float)(i+1);
	}
}

__global__ void
p_extract(const int *A, int *B) {
	int i = blockIdx.x * blockDim.x + threadIdx.x;

	if((i+1) % BLOCK_SIZE == 0) {
        int index = ((i+1) / BLOCK_SIZE) - 1;
        B[index] = A[i];
    }
}

__global__ void
p_addToBlocks(const int *A, int *B) {
	int i = blockIdx.x * blockDim.x + threadIdx.x;

	if(i >= BLOCK_SIZE) {
        int index = (i / BLOCK_SIZE) - 1;
        B[i] = B[i] + A[index];
    }
}

/**
	Serial code for scan
*/
void
s_scan(const int *x, int *y, int len) {
	y[0] = x[0];
	for(int i = 0; i < len; i++) {
		y[i] = y[i-1] + x[i];
	}
}

/**
    Serial code for average
*/
void
s_vectorAverageEfficient(const int *A, float *B, int *C, int numElements, int n) {
	s_scan(A, C, numElements);
	for(int i = 0; i < numElements; i++) {
		if(i >= n) {
			int index = i-n+1;
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
    int numElements = 10000000;
    int windowSize = 5;
    size_t size = numElements * sizeof(int);
    size_t sizeF = numElements * sizeof(float);
    printf("[Vector average of %d elements]\n", numElements);

    // Allocate host input vector A
    int *h_A = (int *)malloc(size);
    // Allocate host input vector B
    float *h_B = (float *)malloc(sizeF);
    // Allocate host input vector C - used for serial version to check parallel
    float *h_C = (float *)malloc(sizeF);
    // Allocate host input vector D - used for serial version to check parallel
    int *h_D = (int *)malloc(size);
    // Allocate host input vector E - used for extract
    int *h_E = (int *)malloc(size);

    // Verify success of allocations
    if(h_A == NULL || h_B == NULL || h_C == NULL || h_D == NULL || h_E == NULL) {
        fprintf(stderr, "Failed to allocate host vectors!\n");
        exit(EXIT_FAILURE);
    } 

    // Initialise host vectors - random ints between 0-9
    for(int i = 0; i < numElements; i++) {
        h_A[i] = rand()%10;
    }

    // Allocate the device input vector A
    int *d_A = NULL;
    err = cudaMalloc((void **)&d_A, size);

    if (err != cudaSuccess) {
		fprintf(stderr, "Failed to allocate device vector A (error code %s)!\n", cudaGetErrorString(err));
		exit(EXIT_FAILURE);
	}
    // Allocate the device input vector B
    int *d_B = NULL;
    err = cudaMalloc((void **)&d_B, size);

    if (err != cudaSuccess) {
		fprintf(stderr, "Failed to allocate device vector B (error code %s)!\n", cudaGetErrorString(err));
		exit(EXIT_FAILURE);
	}
    // Allocate the device input vector C
	int *d_C = NULL;
	err = cudaMalloc((void **)&d_C, size);

	if (err != cudaSuccess) {
		fprintf(stderr, "Failed to allocate device vector C (error code %s)!\n", cudaGetErrorString(err));
		exit(EXIT_FAILURE);
	}

    // Allocate the device input vector E
	int *d_E = NULL;
	err = cudaMalloc((void **)&d_E, size);

	if (err != cudaSuccess) {
		fprintf(stderr, "Failed to allocate device vector E (error code %s)!\n", cudaGetErrorString(err));
		exit(EXIT_FAILURE);
	}

    // Allocate the device input vector E2 - scanned result of extract
	int *d_E2 = NULL;
	err = cudaMalloc((void **)&d_E2, size);

	if (err != cudaSuccess) {
		fprintf(stderr, "Failed to allocate device vector E2 (error code %s)!\n", cudaGetErrorString(err));
		exit(EXIT_FAILURE);
	}

    // Allocate the device input vector E2 - scanned result of extract
	int *d_E3 = NULL;
	err = cudaMalloc((void **)&d_E3, size);

	if (err != cudaSuccess) {
		fprintf(stderr, "Failed to allocate device vector E3 (error code %s)!\n", cudaGetErrorString(err));
		exit(EXIT_FAILURE);
	}

    // Allocate the device input vector E2 - scanned result of extract
	int *d_E4 = NULL;
	err = cudaMalloc((void **)&d_E4, size);

	if (err != cudaSuccess) {
		fprintf(stderr, "Failed to allocate device vector E4 (error code %s)!\n", cudaGetErrorString(err));
		exit(EXIT_FAILURE);
	}

    // Allocate the device input vector B2 - windowed average of b
	float *d_B2 = NULL;
	err = cudaMalloc((void **)&d_B2, sizeF);

	if (err != cudaSuccess) {
		fprintf(stderr, "Failed to allocate device vector B2 (error code %s)!\n", cudaGetErrorString(err));
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
	int threadsPerBlock = BLOCK_SIZE;
    // Note this pattern, based on integer division, for rounding up
    int blocksPerGrid = 1 + ((numElements - 1) / threadsPerBlock);

    printf("Launch  CUDA kernel with %d blocks of %d threads\n", blocksPerGrid, threadsPerBlock);

#ifdef TIMING_SUPPORT
    StopWatchInterface *timer = NULL;
    sdkCreateTimer(&timer);             // create a timer
    sdkStartTimer(&timer);              // start the timer
#endif
#ifdef CUDA_TIMING
    cudaEvent_t start, stop;
    float time;

    cudaEventCreate(&start);
    cudaEventCreate(&stop);

    cudaEventRecord( start, 0 );
#endif

    // Call serial version
    //s_vectorAverageEfficient(h_A, h_C, h_D, numElements, windowSize);
    //s_scan(h_A, h_C, numElements);

    // Call kernel version
    p_scanEfficient<<<blocksPerGrid, threadsPerBlock>>>(d_A, d_B, numElements);
    p_extract<<<blocksPerGrid, threadsPerBlock>>>(d_B, d_E);
    p_scanEfficient<<<blocksPerGrid, threadsPerBlock>>>(d_E, d_E2, (numElements/BLOCK_SIZE));

    if(numElements > BLOCK_SIZE*BLOCK_SIZE) {
        p_extract<<<blocksPerGrid, threadsPerBlock>>>(d_E2, d_E3);
        p_scanEfficient<<<blocksPerGrid, threadsPerBlock>>>(d_E3, d_E4, (numElements/BLOCK_SIZE/BLOCK_SIZE));
        p_addToBlocks<<<blocksPerGrid, threadsPerBlock>>>(d_E4, d_E2);
    }
    
    p_addToBlocks<<<blocksPerGrid, threadsPerBlock>>>(d_E2, d_B);
    p_vectorAverage<<<blocksPerGrid, threadsPerBlock>>>(d_B, d_B2, numElements, windowSize);

    cudaDeviceSynchronize();   // Make sure kernels have finished before stopping timer

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
        fprintf(stderr, "Failed to launch kernel (error code %s)!\n", cudaGetErrorString(err));
        exit(EXIT_FAILURE);
    }

#ifdef TIMING_SUPPORT
    // stop and destroy timer
    sdkStopTimer(&timer);
    double dSeconds = sdkGetTimerValue(&timer)/(1000.0);
    double dNumOps = 1.0e-9 * size;
    double gflops = dNumOps/dSeconds;

    //Log throughput, etc

    printf("Throughput = %.4f GFlops\nTime = %.5f s\nSize = %.5f Gops\n\n",
  	   gflops, dSeconds, dNumOps);
    sdkDeleteTimer(&timer);
#endif

    // Copy the device result vector in device memory to the host result vector
    // in host memory.
    printf("Copy output data from the CUDA device to the host memory\n");
    err = cudaMemcpy(h_B, d_B2, size, cudaMemcpyDeviceToHost);
    if (err != cudaSuccess) {
        fprintf(stderr, "Failed to copy vector B from device to host (error code %s)!\n", cudaGetErrorString(err));
        exit(EXIT_FAILURE);
    }

    // Copy the device result vector in device memory to the host result vector
    // in host memory.
    printf("Copy output data from the CUDA device to the host memory\n");
    err = cudaMemcpy(h_E, d_E2, size, cudaMemcpyDeviceToHost);
    if (err != cudaSuccess) {
        fprintf(stderr, "Failed to copy vector E2 from device to host (error code %s)!\n", cudaGetErrorString(err));
        exit(EXIT_FAILURE);
    }

    // Run serial version to check against parallel
    s_vectorAverageEfficient(h_A, h_C, h_D, numElements, windowSize);
    //s_scan(h_A, h_C, numElements);

/*
    // Print result - Debugging
    printf("Serial Version - Vector Average\n");
    for(int i = 0; i < numElements; i++) {
        printf("%d\n", h_C[i]);
    }
	printf("\n");

	printf("Parallel Version - Vector Average\n");
    for(int i = 0; i < numElements; i++) {
        printf("%lf\n", h_B[i]);
    }
    printf("\n");

	printf("Parallel Version - Extract Result\n");
    for(int i = 0; i < (numElements/BLOCK_SIZE); i++) {
        printf("%d\n", h_E[i]);
    }
*/

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

    err = cudaFree(d_C);
    if (err != cudaSuccess) {
		fprintf(stderr, "Failed to free device vector C (error code %s)!\n", cudaGetErrorString(err));
		exit(EXIT_FAILURE);
    }

    err = cudaFree(d_E);
    if (err != cudaSuccess) {
		fprintf(stderr, "Failed to free device vector E (error code %s)!\n", cudaGetErrorString(err));
		exit(EXIT_FAILURE);
    }

    err = cudaFree(d_E2);
    if (err != cudaSuccess) {
		fprintf(stderr, "Failed to free device vector E2 (error code %s)!\n", cudaGetErrorString(err));
		exit(EXIT_FAILURE);
    }

    err = cudaFree(d_E3);
    if (err != cudaSuccess) {
		fprintf(stderr, "Failed to free device vector E3 (error code %s)!\n", cudaGetErrorString(err));
		exit(EXIT_FAILURE);
    }

    err = cudaFree(d_E4);
    if (err != cudaSuccess) {
		fprintf(stderr, "Failed to free device vector E4 (error code %s)!\n", cudaGetErrorString(err));
		exit(EXIT_FAILURE);
    }

    err = cudaFree(d_B2);
    if (err != cudaSuccess) {
		fprintf(stderr, "Failed to free device vector B2 (error code %s)!\n", cudaGetErrorString(err));
		exit(EXIT_FAILURE);
    }

    // Free host memory
    free(h_A);
    free(h_B);
    free(h_C);
    free(h_D);
    free(h_E);

    // Reset the device and exit
    err = cudaDeviceReset();

    if (err != cudaSuccess) {
        fprintf(stderr, "Failed to reset the device! error=%s\n", cudaGetErrorString(err));
        exit(EXIT_FAILURE);
    }
    printf("Done\n");
    return 0;
}
