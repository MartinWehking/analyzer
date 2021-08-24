//PARAM: --disable ana.int.interval --sets sem.int.signed_overflow assume_wraparound
#include <assert.h>

int main(){
    int a = 0;

    // maximum value for long long
    signed long long s = 9223372036854775807;
    assert(s > 9223372036854775806);

    signed long long t = s + 2;
    // Signed overflow - The following assertion only works with sem.int.signed_overflow set to assume_wraparound
    assert(t == -9223372036854775807);

    return 0;
}
