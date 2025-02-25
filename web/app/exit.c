#include <stdlib.h>

/* The builtin Haskll exitSuccess bit will hang waiting for all the threads to
 * clean up. Needless to say, this is not what we want. */
int reallyExit(void)
{
        exit(0);
}
