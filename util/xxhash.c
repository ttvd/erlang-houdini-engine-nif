#include "../deps/xxhash/xxhash.h"

#include <stdio.h>
#include <string.h>

int main(int argc, const char** argv)
{
    unsigned int hash = 0u;

    if(argc >= 2)
    {
        hash = XXH32(argv[1], strlen(argv[1]), 0);
    }

    printf("%u\n", hash);
    return 0;
}
