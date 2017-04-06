//
//  allocators.h
//
//  Created by silviu on 3/29/17.
//  Copyright © 2017 silviu. All rights reserved.
//

#ifndef ERLXML_C_SRC_ALLOCATORS_H_
#define ERLXML_C_SRC_ALLOCATORS_H_

#include <stdlib.h>

void* erlxml_allocate(size_t size);
void erlxml_deallocate(void* ptr);

#endif
