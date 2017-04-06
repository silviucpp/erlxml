//
//  allocators.cc
//
//  Created by silviu on 3/29/17.
//  Copyright © 2017 silviu. All rights reserved.
//

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

