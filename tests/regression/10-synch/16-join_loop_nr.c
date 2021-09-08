// SKIP PARAM: --set ana.activated[+] thread
#include <pthread.h>
#include <stdio.h>

int myglobal;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&mutex);
  myglobal=myglobal+1; // NORACE
  pthread_mutex_unlock(&mutex);
  return NULL;
}

int main(void) {
  pthread_t id[10];
  int i;
  for (i=0; i<10; i++)
    pthread_create(&id[i], NULL, t_fun, NULL);
  pthread_mutex_lock(&mutex);
  myglobal=myglobal+1; // NORACE
  pthread_mutex_unlock(&mutex);
  for (i=0; i<10; i++)
    pthread_join(id[i], NULL);
  myglobal=myglobal+1; // NORACE
  return 0;
}
