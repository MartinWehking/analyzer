// PARAM: --sets otherfun[+] f
// no earlyglobs!
#include <pthread.h>
#include <assert.h>

int g = 0;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void f() {
  pthread_mutex_lock(&A);
  while (g) {
    // should be unreachable
  }
  assert(g == 0); // should be reachable
}