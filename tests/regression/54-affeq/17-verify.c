//SKIP PARAM: --set ana.activated[+] affeq  --set ana.matrix "array" --sem.int.signed_overflow "assume_none"  --enable ana.int.interval
// Mistake in leq check lead to verify error

int main() {
  int n, a, b;

  n = a - b;
  while (n--) {

  }

  return 0;
}
