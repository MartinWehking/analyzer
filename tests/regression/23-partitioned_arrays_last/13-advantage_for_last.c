// PARAM: --set solver td3 --enable ana.int.interval  --set exp.partition-arrays.keep-expr "last" --enable exp.partition-arrays.enabled --set ana.activated "['base','threadid','threadflag','escape','expRelation','octagon','mallocWrapper']" --set exp.privatization none
void main(void) {
  example1();
}

void example1(void) {
    int a[42];
    a[40] = 2;
    int i = 0;

    while(i < 41) {
        a[i] = 0;
        i++;
    }

    assert(a[2] == 0);
    assert(a[3] == 0);
}
