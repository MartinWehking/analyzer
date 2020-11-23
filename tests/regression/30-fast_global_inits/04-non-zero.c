// PARAM: --sets solver td3 --enable ana.int.interval --enable exp.partition-arrays.enabled --enable exp.fast_global_inits
// This checks that partitioned arrays and fast_global_inits are no longer incompatible
int global_array[5] = {9, 0, 3, 42, 11};
int global_array_multi[2][5] =  {{9, 0, 3, 42, 11}, {9, 0, 3, 42, 11}};

int main(void) {
  assert(global_array[0] == 9);  //UNKNOWN
  assert(global_array[1] == 0);  //UNKNOWN
  assert(global_array[2] == 3);  //UNKNOWN
  assert(global_array[3] == 42); //UNKNOWN
  assert(global_array[3] == 11); //UNKNOWN

  assert(global_array_multi[0][0] == 9);  //UNKNOWN
  assert(global_array_multi[0][1] == 0);  //UNKNOWN
  assert(global_array_multi[0][2] == 3);  //UNKNOWN
  assert(global_array_multi[0][3] == 42); //UNKNOWN
  assert(global_array_multi[0][3] == 11); //UNKNOWN


  assert(global_array_multi[1][0] == 9);  //UNKNOWN
  assert(global_array_multi[1][1] == 0);  //UNKNOWN
  assert(global_array_multi[1][2] == 3);  //UNKNOWN
  assert(global_array_multi[1][3] == 42); //UNKNOWN
  assert(global_array_multi[1][3] == 11); //UNKNOWN
}
