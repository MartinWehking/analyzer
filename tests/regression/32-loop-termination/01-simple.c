// PARAM: --set ana.activated "['base','threadid','threadflag','term','mallocWrapper', 'poly', 'octy']" --enable dbg.debug --enable ana.int.interval --sets solver slr3

int main(void) {
    int x, y, r;
    x = 5;
    y = 3;
    r = x + y; // r is 8
    while (x != y) {
        //assert(r > 0);
        if (y > x) 
            y = y - x; //2nd iter: y = 1
        else 
            x = x - y; // 1st iter goes here, x = 2
        //assert(r > x + y); // 8 > 3+2,  5 > 1+3,  4 > 1+2, 3 > 1+1, 2 > 1 + 0, 1 > 0
        r = x + y; 
    }
    return 0;
}