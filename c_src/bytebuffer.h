#ifndef ERLTLS_C_SRC_BYTEBUFFER_H_
#define ERLTLS_C_SRC_BYTEBUFFER_H_

#include "macros.h"

#include <stdlib.h>
#include <stdint.h>

class ByteBuffer
{

public:

    ByteBuffer();
    ByteBuffer(size_t size);
    ByteBuffer(const uint8_t* bytes, size_t len);

    ~ByteBuffer();

    const uint8_t* Data() const { return bytes_ + start_; }
    size_t Length() const { return end_ - start_; }
    size_t Capacity() const { return size_ - start_; }

    bool ReadBytes(uint8_t* val, size_t len);
    void WriteBytes(const uint8_t* val, size_t len);

    uint8_t* ReserveWriteBuffer(size_t len);
    void Resize(size_t size);
    bool Consume(size_t size);
    void Clear();

private:

    void Construct(const uint8_t* bytes, size_t size);

    uint8_t* bytes_;
    size_t size_;
    size_t start_;
    size_t end_;

    DISALLOW_COPY_AND_ASSIGN(ByteBuffer);
};

#endif
