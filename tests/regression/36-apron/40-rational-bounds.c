// SKIP PARAM: --set ana.activated[+] apron
extern int __VERIFIER_nondet_int();

#include <assert.h>

void main() {
  int x = __VERIFIER_nondet_int(); //rand
  {
//   if (1 <= x) {
//   if (-10 <= x) {
    if (x <= 10) {
      assert((x / 3) <= 4);
      assert((x / 3) <= 3); // TODO: why does apron think the upper bound is 13/3 if no (positive) lower bound is known?
    }
  }
}
