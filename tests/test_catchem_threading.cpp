// Verifies that the Kokkos OpenMP host backend really executes test kernels
// across multiple threads, and that the thread count is the one pinned by
// CATCHEM_TEST_OMP_THREADS (delivered to every Kokkos-capable test as
// OMP_NUM_THREADS via CTest, and to this test as EXPECTED_OMP_THREADS via
// catchem_test_config.hpp). Equality is asserted deliberately: an unpinned
// run inherits the build machine's core count and must fail here.
#include "catchem_kokkos_compat.hpp"
#include "catchem_test_config.hpp"
#include <iostream>

#ifdef KOKKOS_ENABLE_OPENMP
#include <omp.h>
#endif

int main(int argc, char* argv[]) {
#ifndef KOKKOS_ENABLE_OPENMP
    std::cerr << "FAIL: Kokkos was built without the OpenMP backend; the "
              << "multithreaded test environment requires it" << std::endl;
    return 1;
#else
    Kokkos::initialize(argc, argv);
    int failures = 0;
    {
        // Record which backend and thread count actually ran in every log.
        Kokkos::print_configuration(std::cout, true);

        const int expected = catchem::test::EXPECTED_OMP_THREADS;
        const int concurrency = Kokkos::DefaultHostExecutionSpace().concurrency();
        std::cout << "Expected OpenMP threads: " << expected << ", host concurrency: " << concurrency << std::endl;
        if (concurrency != expected) {
            std::cerr << "FAIL: host concurrency " << concurrency << " != configured CATCHEM_TEST_OMP_THREADS "
                      << expected << " (thread count not pinned by the test environment?)" << std::endl;
            ++failures;
        }

        // Mark every OpenMP thread that receives work from a parallel_for.
        // Static scheduling over a range this size guarantees each of the
        // pinned threads a chunk.
        const int n_iterations = 10000;
        Kokkos::View<int*, Kokkos::DefaultHostExecutionSpace> participated("participated", concurrency);
        Kokkos::parallel_for(
            "catchem_threading_mark_threads", Kokkos::RangePolicy<Kokkos::DefaultHostExecutionSpace>(0, n_iterations),
            KOKKOS_LAMBDA(int) { participated(omp_get_thread_num()) = 1; });
        Kokkos::fence();

        int distinct = 0;
        for (int tid = 0; tid < concurrency; ++tid) {
            distinct += participated(tid);
        }
        std::cout << "Distinct OpenMP threads that executed kernel work: " << distinct << std::endl;
        if (distinct < 2) {
            std::cerr << "FAIL: only " << distinct << " thread(s) participated in the parallel_for; "
                      << "expected at least 2" << std::endl;
            ++failures;
        }

        if (failures == 0) {
            std::cout << "PASS: kernel work executed across " << distinct << " OpenMP threads (pinned to " << expected
                      << ")" << std::endl;
        }
    }
    Kokkos::finalize();
    return failures == 0 ? 0 : 1;
#endif
}
