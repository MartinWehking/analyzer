// PARAM: --set solver td3 --enable ana.int.interval --enable exp.partition-arrays.enabled  --set ana.activated "['base','threadid','threadflag','expRelation','octagon','mallocWrapper']" --set exp.privatization none
// These examples were cases were we saw issues of not reaching a fixpoint during development of the octagon domain. Since those issues might
// resurface, these tests without asserts are included
int main(int argc, char const *argv[])
{
    int a[256];
    int i = 0;
    int zPend = 0;
    int ll_i;
    int nblock;

    while (i < nblock)
    { // TO-DO: Here
        if (a[0] == ll_i) // this needs to be var == var for the problem to occur
        {
            zPend++;
        }
        else
        {
            a[0] = 8;
        }

        i++;
    }

    return 0;
}
