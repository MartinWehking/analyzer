// PARAM: --set solver td3 --enable ana.int.interval --set ana.base.arrays.domain partitioned  --set ana.activated "['base','threadid','threadflag','expRelation','mallocWrapper','var_eq']" --set ana.base.privatization none
int global;

int main(void)
{
    example1();
    example2();
    example3();
    example4();
    example5();
    example6();
    example7();
    example8();
    example9();
    example10();
    return 0;
}

// Simple example
void example1(void)
{
    int top;
    int top2;
    int arr[10];

    arr[top] = 42;
    top2 = top;
    assert(arr[top2] == 42);
}
