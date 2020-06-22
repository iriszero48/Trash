#include <stdbool.h>
#include <string.h>

bool solution(const char *string, const char *ending)
{
	return strlen(string) >= strlen(ending) ? !strcmp(string + strlen(string) - strlen(ending), ending) : false;
}
