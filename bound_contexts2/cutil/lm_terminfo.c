/*
 * Simple Terminfo Interface for ML
 * Copyright(c) 2002 Justin David Smith, Caltech
 */
#include <stdlib.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>


#if NCURSES


/* Headers which are readline-specific must be included here. */
#include <ncurses.h>
#include <term.h>


static int loaded_terminfo = 0;

static int load_terminfo() {

   char *termname = NULL;

   /* Check to see if we already loaded the terminal data */
   if(loaded_terminfo) return(0);
   
   /* We haven't loaded anything yet (or we had an error). */
   if(setupterm(NULL, 1, NULL) == OK) {
      /* We were successful! */
      loaded_terminfo = 1;
      return(0);
   }
   
   /* Failure. */
   return(-1);

}

#endif /* NCURSES support? */


/*
 * Read the indicated terminfo by string.
 */
value caml_tgetstr(value id) {

   CAMLparam1(id);
   CAMLlocal1(result);
   char *termdata = NULL;

   /* Lookup the requested capability name.  Note that we only get terminfo
      if we compiled with readline support; otherwise it will not be linked
      in.  */
#if NCURSES
   if(load_terminfo() == 0) {
      termdata = tigetstr(String_val(id));
   }
#endif

   /* Note that tigetstr will return either 0 or -1 on error. */
   if(termdata == NULL || termdata == (char *)(-1)) {
      result = copy_string("");
   } else {
      result = copy_string(termdata);
      /* apparently we're not supposed to free termdata here */
      /* TEMP:  I cannot find specs on this! */
      //free(termdata);
   }

   /* Return the result */
   CAMLreturn(result);

}
