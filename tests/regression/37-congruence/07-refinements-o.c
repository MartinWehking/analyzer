// PARAM: --enable ana.int.congruence
int main() {
    int top;
    int i = 0;
    if(top % 17 == 3) {
        assert(top%17 ==3);
        if(top %17 != 3) {
            i = 12;
        } else {

        }
    }
    assert(i ==0);

    if(top % 17 == 0) {
        assert(top%17 == 0);
        if(top %17 != 0) {
            i = 12;
        }
    }
    assert(i == 0);

    if(top % 3 == 17) {
        assert(top%17 == 3); //UNKNOWN!
    }

}
