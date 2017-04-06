#include "allocators.h"
#include "erl_nif.h"

void* erlxml_allocate(size_t size)
{
    return enif_alloc(size);
}

void erlxml_deallocate(void* ptr)
{
    enif_free(ptr);
}

