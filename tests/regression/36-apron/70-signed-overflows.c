// SKIP PARAM: --set ana.activated[+] apron --set sem.int.signed_overflow assume_none --disable ana.int.interval
// copied from signed-overflows/intervals for apron
int main(void) {
    int x = 0;
    while(x != 42) {
        x++;
        assert(x >= 1);
    }

}
