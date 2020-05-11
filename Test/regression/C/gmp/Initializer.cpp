
#ifndef INITIALIZER_H
#define INITIALIZER_H
#include <gmp.h>

mpq_t array[2];

class Initializer {
  public:
    Initializer() {
      mpq_init(array[0]);
      mpq_set_d(array[0],10.0);
      mpq_init(array[1]);
      mpq_set_d(array[1],2.0);
    }
    //int blah;
};
Initializer *i = new Initializer();

/*keep_i () {
  i.blah = 6;
}*/
#endif