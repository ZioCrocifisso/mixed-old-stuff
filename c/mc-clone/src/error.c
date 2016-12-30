#include <stdio.h>
#include <stdlib.h>

#include "error.h"

void mc_warning(char *str)
{
	fprintf(stderr, "%s\n", str);
}

void mc_error(int error, char *str)
{
	mc_warning(str);
	exit(error);
}
