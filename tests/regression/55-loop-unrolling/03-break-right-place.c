// PARAM: --set exp.unrolling-factor 5
#include<assert.h>

int main(void) {
    int i = 0;
    int j = 0;

    while(i< 17) {
        if (j==0) {
            j = 1;
            // the break is not just out of this unrolling, but out of the entire loop!
            break;
        }
        i++;
    }

    assert(i == 0);
}
