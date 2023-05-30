#include <vector>
#include <cassert>
#include <cstdlib>
#include <cstdio>

// Benchmarks a single permutation kernel accross multiple runs.
// Returns the average running time in milliseconds.
float benchmark(void* kernel, dim3 block_count, dim3 block_size, int input_size, int runs = 1) 
{
	assert(runs > 0);

    // Generate the input array.
    std::vector<int> input_h;
	for (int i = 0; i < input_size; i++) {
		input_h.push_back(rand());
	}

    // Copy the input to the GPU.
    int *input_d, *output_d;
    cudaMalloc(&input_d, input_size * sizeof(int));
    cudaMalloc(&output_d, input_size * sizeof(int));
    cudaMemcpy(input_d, input_h.data(), input_size * sizeof(int), cudaMemcpyHostToDevice);

    // Get the time measurements.
    std::vector<float> times;
    for (int i = 0; i < runs; i++) {
    	cudaEvent_t start, stop;
    	cudaEventCreate(&start);
    	cudaEventCreate(&stop);

		std::vector<void*> args = { (void*)&input_d, (void*)&output_d };

    	cudaEventRecord(start);
    	cudaLaunchKernel(kernel, block_count, block_size, args.data());
    	cudaEventRecord(stop);

        cudaEventSynchronize(stop);
	    float milliseconds = 0;
	    cudaEventElapsedTime(&milliseconds, start, stop);
	    times.push_back(milliseconds);
	}

	// No need to copy the output back to the CPU, simply free the GPU memory.
    cudaFree(input_d);
    cudaFree(output_d);

    // Compute the average time per run.
    float total = 0;
    for (float t : times) total += t;
    return total / (float)runs;
}


// The copy kernel we use to get the maximum effective bandwidth. 
// You should launch exactly as many threads as there are array elements.
__global__ void copy_kernel(const int* input, int* output)
{
    int i = blockIdx.x * blockDim.x + threadIdx.x;
    output[i] = input[i];
}

// If you wish to benchmark any of the generated permutation kernels,
// paste the generated CUDA code here.


// We give as an example the code to benchmark the copy kernel as well commented out
// code to benchmark a BPC kernel (you have to fill in the correct dimensions).
int main()
{
	int input_size = 1 << 25;
	int runs = 100;	

	float copy_time = benchmark((void*)copy_kernel, { (unsigned int)input_size / 1024, 1, 1 }, { 1024, 1, 1 }, input_size, runs);  
	//float bpc_time = benchmark((void*)my_bpc_kernel, { ???, 1, 1 }, { ???, ???, 1 }, input_size, runs);  
	
	printf("Copy time (input size=%d) : %.3fms\n", input_size, copy_time);
	//printf("BPC time (input size=%d) : %.3fms\n", input_size, bpc_time);

	return 0;
}


