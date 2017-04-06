//
//  xmlstreamparser.cc
//
//  Created by silviu on 3/23/17.
//  Copyright © 2017 silviu. All rights reserved.

#include "xmlstreamparser.h"
#include <assert.h>
#include <algorithm>

//http://pugixml.org/docs/manual.html
//Limitations for stanza detection algorithm (streaming mode):
//  1. not supporting cdata
//  2. not supporting comments with special xml charachters inside
//  3. not supporting doctype

const int kDefaultBufferSize = 1024;

// whitespace (space \n \r \t) lookup table

const uint8_t kLookupWhitespace[256] = {
    0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  1,  0,  0,  1,  0,  0,  // 0
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  // 1
    1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  // 2
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  // 3
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  // 4
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  // 5
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  // 6
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  // 7
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  // 8
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  // 9
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  // A
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  // B
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  // C
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  // D
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  // E
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0   // F
};

// match - ! ? and / . we don't increase the nested level for those in case are before >
// and also we ignore the stanza's that has only one element of this type (header or comment)

const uint8_t kLookupSkipTag[256] = {
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  //0
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  //1
    0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  1,  //2
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  //3
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  //4
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  //5
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  //6
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  //7
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  //8
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  //9
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  //A
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  //B
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  //C
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  //D
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  //E
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  //F
};

XmlStreamParser::XmlStreamParser(bool skip_root, size_t max_stanza, XmlStreamElementHandler handler) :
    skip_root_(skip_root),
    max_stanza_bytes_(max_stanza),
    handler_(handler),
    nested_level_(0),
    last_char_(0),
    end_begin_detected_(false)
{
    if(skip_root_)
        nested_level_ = -1;
}

XmlStreamParser::~XmlStreamParser()
{

}

void XmlStreamParser::Cleanup()
{
    buffer_.Clear();
    buffer_.Resize(kDefaultBufferSize);
    nested_level_ = 0;
    last_char_ = 0;
    end_begin_detected_ = false;
}

XmlStreamParser::parse_result XmlStreamParser::FeedData(const uint8_t* data, size_t length, void* user_data)
{
    if(length == 0)
        return kParseOk;

    size_t buffered_bytes = buffer_.Length();

    size_t remaining_stanza_bytes = max_stanza_bytes_ > 0 ? std::min(max_stanza_bytes_ - buffered_bytes, length) : length;
    size_t end_stanza_index = FindStanzaUpperLimit(data, remaining_stanza_bytes);

    if(end_stanza_index == remaining_stanza_bytes)
    {
        if(remaining_stanza_bytes != length)
        {
            Cleanup();
            return kParseStanzaLimitHit;
        }

        if(nested_level_ == -1 && skip_root_ == false)
        {
            //finished the stream
            Cleanup();
            return kParseOk;
        }

        buffer_.WriteBytes(data, length);
        last_char_ = data[length-1];
        return kParseOk;
    }

    end_stanza_index++;

    if(buffered_bytes)
    {
        buffer_.WriteBytes(data, end_stanza_index);

        if(!PushStanza(const_cast<uint8_t*>(buffer_.Data()), buffer_.Length(), user_data))
        {
            Cleanup();
            return kParseInvalidXml;
        }

        buffer_.Clear();
        buffer_.Resize(kDefaultBufferSize);
    }
    else
    {
        if(!PushStanza(const_cast<uint8_t*>(data), static_cast<size_t>(end_stanza_index), user_data))
        {
            Cleanup();
            return kParseInvalidXml;
        }
    }

    size_t remaining = length - end_stanza_index;

    if(remaining)
        return FeedData(data+end_stanza_index, remaining, user_data);

    return kParseOk;
}

size_t XmlStreamParser::FindStanzaUpperLimit(const uint8_t* ptr, size_t size)
{
    size_t index = 0;

    if(last_char_ == '<' && ptr[index] == '/')
    {
        end_begin_detected_ = true;
        index++;
    }

    for(; index < size; index++)
    {
        switch (ptr[index])
        {
            case '<':

                if(index < size -1 && ptr[index+1] == '/')
                {
                    end_begin_detected_ = true;
                    index++;
                }

                break;

            case '>':

                if(end_begin_detected_)
                {
                    nested_level_--;
                    end_begin_detected_ = false;
                }
                else
                {
                    int8_t last = index > 0 ? ptr[index-1] : last_char_;

                    if(kLookupSkipTag[last] == 0)
                        nested_level_++;
                }

                if(nested_level_ == 0)
                    return index;

                break;
        }
    }

    return index;
}

bool XmlStreamParser::PushStanza(uint8_t* buffer, size_t length, void* user_data)
{
    if(skip_root_)
    {
        //drop all bytes so far
        skip_root_ = false;
        end_begin_detected_ = false;
        last_char_ = 0;
        return true;
    }

    size_t skip_bytes = 0;

    while (kLookupWhitespace[static_cast<unsigned char>(buffer[skip_bytes])])
        skip_bytes++;

    length -= skip_bytes;

    if(length < 4)
        return false;

    // don't parse anything in case we have a header or comment as first element

    if(!kLookupSkipTag[buffer[skip_bytes+1]])
    {
        if( pugi_doc_.load_buffer_inplace(buffer+skip_bytes, length).status != pugi::status_ok)
            return false;

        handler_(user_data, pugi_doc_);
    }

    end_begin_detected_ = false;
    last_char_ = 0;
    assert(nested_level_ == 0);
    return true;
}

void XmlStreamParser::Reset(bool skip_root)
{
    skip_root_ = skip_root;
    Cleanup();

    if(skip_root_)
        nested_level_ = -1;
}
