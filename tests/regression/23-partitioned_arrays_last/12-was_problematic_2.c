// PARAM: --sets solver td3 --enable ana.int.interval  --enable exp.partition-arrays.enabled --sets exp.partition-arrays.keep-expr "last"  --set ana.activated "['base','threadid','threadflag','escape','expRelation','mallocWrapper']" --sets exp.privatization none
int main(void)
{
  int arr[260];
  int n;

  n = 5;
  arr[0] = 0;

  while (n > 1) { //here
    arr[1] = 7;
    n--;
  }

  return 0;
}
