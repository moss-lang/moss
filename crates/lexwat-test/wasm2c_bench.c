/* Benchmark the wasm2c-translated lexer compiled to native code by clang/gcc.
   Mirrors the Rust bench (same corpus, 100 passes): reports MB/s + ns/token.

   This is the "compile wasm to native via LLVM" data point. See README.md for
   the full pipeline; in short:

     wasm-opt lex_selfsource.wat -o self.wasm        # binaryen: wat -> wasm
     wasm2c self.wasm -n lex -o self.c               # wabt: wasm -> C
     cargo test -p lexwat-test --release --test bench dump_corpus -- --ignored
     clang -O3 -I. -I<wabt/include> -I<wabt/wasm2c> \
         wasm2c_bench.c self.c \
         <wabt/wasm2c>/wasm-rt-impl.c <wabt/wasm2c>/wasm-rt-mem-impl.c -o bench
     ./bench
*/
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "self.h"

#define SRC_OFFSET 1024 /* lex_selfsource keeps source at offset 1024 */

int main(void) {
    FILE *f = fopen("/tmp/corpus.bin", "rb");
    if (!f) { perror("corpus (run the dump_corpus test first)"); return 1; }
    fseek(f, 0, SEEK_END); long n = ftell(f); fseek(f, 0, SEEK_SET);
    uint8_t *buf = malloc(n);
    if (fread(buf, 1, n, f) != (size_t)n) { return 1; }
    fclose(f);

    wasm_rt_init();
    w2c_lex inst;
    wasm2c_lex_instantiate(&inst);
    wasm_rt_memory_t *mem = w2c_lex_memory(&inst);
    memcpy(mem->data + SRC_OFFSET, buf, n);

    const int iters = 100;
    unsigned long long acc = 0;

    w2c_lex_reset(&inst); w2c_lex_set_len(&inst, (uint32_t)n);
    { uint32_t id; do { id = w2c_lex_next_token(&inst); acc++; } while ((int32_t)id > 0); }

    struct timespec t0, t1;
    clock_gettime(CLOCK_MONOTONIC, &t0);
    for (int it = 0; it < iters; it++) {
        w2c_lex_reset(&inst);
        w2c_lex_set_len(&inst, (uint32_t)n);
        uint32_t id;
        do { id = w2c_lex_next_token(&inst); acc++; } while ((int32_t)id > 0);
    }
    clock_gettime(CLOCK_MONOTONIC, &t1);

    double dt = (t1.tv_sec - t0.tv_sec) + (t1.tv_nsec - t0.tv_nsec) / 1e9;
    printf("  wasm2c native  %8.1f MB/s   %6.1f ns/token   (%.1f ms for %d iters, %llu tok)\n",
           (double)n * iters / dt / 1e6, dt * 1e9 / (double)acc, dt * 1000, iters, acc);

    wasm2c_lex_free(&inst);
    wasm_rt_free();
    free(buf);
    return 0;
}
