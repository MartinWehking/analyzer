// SKIP PARAM: --set ana.activated[+] apron --enable ana.int.interval
extern int __VERIFIER_nondet_int();

#include<pthread.h>
int global = 0;

void *t_fun(void *arg)
{
    global = 5;
    assert(1);
}

int main(void)
{
    pthread_t t;
    int i = __VERIFIER_nondet_int(); //rand

    if(i < 1) {
        pthread_create(&t, ((void *)0), t_fun, ((void *)0));
        assert(global == 0); //UNKNOWN!
        i++;
    } else {
        global = 17;
    }

    assert(global <= 5); //UNKNOWN!
    return 0;
}
