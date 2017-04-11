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

XmlStreamParser::XmlStreamParser(size_t max_stanza, XmlStartStreamHandler start_h, XmlEndStreamHandler end_h, XmlStreamElementHandler el_h) :
    process_root_(true),
    max_stanza_bytes_(max_stanza),
    start_stream_handler_(start_h),
    end_stream_handler_(end_h),
    element_handler_(el_h),
    nested_level_(-1),
    last_char_(0),
    end_begin_detected_(false)
{

}

XmlStreamParser::~XmlStreamParser()
{

}

void XmlStreamParser::Cleanup()
{
    buffer_.Clear();
    buffer_.Resize(kDefaultBufferSize);
    nested_level_ = -1;
    process_root_ = true;
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

        if(nested_level_ < -1)
        {
            Cleanup();
            return kParseInvalidXml;
        }

        if(nested_level_ == -1 && process_root_ == false)
        {
            end_stream_handler_(user_data, root_name_);
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

        if(!PushStanza(const_cast<uint8_t*>(buffer_.Data()), buffer_.Length(), user_data, false))
        {
            Cleanup();
            return kParseInvalidXml;
        }

        buffer_.Clear();
        buffer_.Resize(kDefaultBufferSize);
    }
    else
    {
        if(!PushStanza(const_cast<uint8_t*>(data), static_cast<size_t>(end_stanza_index), user_data, true))
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

bool XmlStreamParser::PushStanza(uint8_t* buffer, size_t length, void* user_data, bool copy)
{
    if(process_root_)
        return ProcessRootElement(buffer, length, user_data);

    size_t skip_bytes = 0;

    while (skip_bytes < length && kLookupWhitespace[static_cast<unsigned char>(buffer[skip_bytes])])
        skip_bytes++;

    length -= skip_bytes;

    if(length < 4)
        return false;

    // don't parse anything in case we have a header or comment as first element
    // for this reason we need to skip all spaces

    if(!kLookupSkipTag[buffer[skip_bytes+1]])
    {
        pugi::xml_parse_status result;

        if(copy)
            result = pugi_doc_.load_buffer(buffer+skip_bytes, length).status;
        else
            result = pugi_doc_.load_buffer_inplace(buffer+skip_bytes, length).status;

        if(result != pugi::status_ok)
            return false;

        element_handler_(user_data, pugi_doc_);
    }

    end_begin_detected_ = false;
    last_char_ = 0;
    assert(nested_level_ == 0);
    return true;
}

bool XmlStreamParser::ProcessRootElement(uint8_t* buffer, size_t length, void* user_data)
{
    if(!length)
        return false;

    ByteBuffer rootbuff;
    rootbuff.WriteBytes(buffer, length-1);
    rootbuff.WriteBytes(reinterpret_cast<const uint8_t*>("/>"), 2);

    pugi::xml_parse_status result = pugi_doc_.load_buffer_inplace(const_cast<uint8_t*>(rootbuff.Data()), rootbuff.Length()).status;

    if(result != pugi::status_ok)
        return false;

    if(!start_stream_handler_(user_data, pugi_doc_))
        return false;

    //drop all bytes so far
    root_name_ = pugi_doc_.document_element().name();
    process_root_ = false;
    end_begin_detected_ = false;
    last_char_ = 0;

    return true;
}

void XmlStreamParser::Reset()
{
    Cleanup();
}
