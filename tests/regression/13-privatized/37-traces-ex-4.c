#include <pthread.h>
#include <assert.h>

int g = 2; // matches expected precise read
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t B = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&B);
  pthread_mutex_lock(&A);
  g = 1;
  pthread_mutex_unlock(&A);
  pthread_mutex_lock(&A);
  g = 2;
  pthread_mutex_unlock(&A);
  pthread_mutex_unlock(&B);
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  pthread_mutex_lock(&B);
  pthread_mutex_lock(&A);
  assert(g == 2);
  pthread_mutex_unlock(&A);
  pthread_mutex_unlock(&B);
  return 0;
}
