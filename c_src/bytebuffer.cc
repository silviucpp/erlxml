#include "bytebuffer.h"
#include "allocators.h"

#include <string.h>
#include <algorithm>

ByteBuffer::ByteBuffer()
{
    Construct(NULL, 1024);
}

ByteBuffer::ByteBuffer(size_t size)
{
    Construct(NULL, size);
}

ByteBuffer::ByteBuffer(const uint8_t* bytes, size_t len)
{
    Construct(bytes, len);
}

void ByteBuffer::Construct(const uint8_t* bytes, size_t len)
{
    start_ = 0;
    size_ = len;
    bytes_ = static_cast<uint8_t*>(erlxml_allocate(size_));

    if (bytes)
    {
        end_ = len;
        memcpy(bytes_, bytes, end_);
    }
    else
    {
        end_ = 0;
    }
}

ByteBuffer::~ByteBuffer()
{
    erlxml_deallocate(bytes_);
}

bool ByteBuffer::ReadBytes(uint8_t* val, size_t len)
{
    if (len > Length())
        return false;

    memcpy(val, bytes_ + start_, len);
    start_ += len;
    return true;
}

void ByteBuffer::WriteBytes(const uint8_t* val, size_t len)
{
    memcpy(ReserveWriteBuffer(len), val, len);
}

uint8_t* ByteBuffer::ReserveWriteBuffer(size_t len)
{
    if (Length() + len > Capacity())
        Resize(Length() + len);

    uint8_t* start = bytes_ + end_;
    end_ += len;
    return start;
}

void ByteBuffer::Resize(size_t size)
{
    size_t len = std::min(end_ - start_, size);
    if (size <= size_)
    {
        // Don't reallocate, just move data backwards
        memmove(bytes_, bytes_ + start_, len);
    }
    else
    {
        // Reallocate a larger buffer.
        size_ = std::max(size, 3 * size_ / 2);
        uint8_t* new_bytes = static_cast<uint8_t*>(erlxml_allocate(size_));
        memcpy(new_bytes, bytes_ + start_, len);

        erlxml_deallocate(bytes_);
        bytes_ = new_bytes;
    }

    start_ = 0;
    end_ = len;
}

bool ByteBuffer::Consume(size_t size)
{
    if (size > Length())
        return false;

    start_ += size;
    return true;
}

void ByteBuffer::Clear()
{
    start_ = end_ = 0;
}
