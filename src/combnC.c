#include <Rdefines.h>
#include <R.h>
#include "shd_print.h"
#include "combnPrimC.h"


void combnC(int *nsel, int *ncand, int *nset, int *ans){

  int ii, ofs, jj, kk;
  int *list;
  //  int ans_len = *nset * *nsel;

  list = (int *) R_alloc(*nsel, sizeof(int));
  for (ii=0;ii<*nsel;ii++)
    list[ii] = 0;

/*   printveci(list, nsel); */
/*   Rprintf("*nset: %i\n", *nset); */

  for (ii=0; ii<*nset; ii++){
    sla_combn__(nsel, ncand, list, &jj); 
/*     printveci(list, nsel); */
    ofs = ii * *nsel;
    for (kk=0;kk<*nsel;kk++)
      ans[ofs+kk] = list[kk];
/*     printveci(ans, &ans_len); */
  }
}