#include "xmlstreamparser.h"
#include <assert.h>
#include <algorithm>

//http://pugixml.org/docs/manual.html
//Limitations for stanza detection algorithm (streaming mode):
//  1. not supporting cdata
//  2. not supporting comments with special xml charachters inside
//  3. not supporting doctype

const size_t kDefaultBufferSize = 1024;

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

XmlStreamParser::XmlStreamParser(size_t max_stanza, bool strip_invalid_utf8, XmlStartStreamHandler start_h, XmlEndStreamHandler end_h, XmlStreamElementHandler el_h) :
    process_root_(true),
    max_stanza_bytes_(max_stanza),
    strip_invalid_utf8_(strip_invalid_utf8),
    start_stream_handler_(start_h),
    end_stream_handler_(end_h),
    element_handler_(el_h),
    nested_level_(-1),
    last_start_tag_index_(-1),
    first_start_tag_index_(-1)
{

}

XmlStreamParser::~XmlStreamParser()
{

}

XmlStreamParser::parse_result XmlStreamParser::FeedData(const uint8_t* data, size_t length, void* user_data)
{
    size_t last_index = buffer_.Length();
    buffer_.WriteBytes(data, length);
    parse_result result = DoProcess(last_index, buffer_.Length(), user_data);

    if(result == kParseOk && buffer_.Capacity() > kDefaultBufferSize)
        buffer_.Resize(std::max(buffer_.Length(), kDefaultBufferSize));

    return result;
}

XmlStreamParser::parse_result XmlStreamParser::DoProcess(size_t start, size_t end, void* user_data)
{
    uint8_t* ptr = const_cast<uint8_t*>(buffer_.Data());
    size_t max_end_position = max_stanza_bytes_ > 0 ? std::min(max_stanza_bytes_, end) : end;

    int64_t end_stanza_index = FindStanzaUpperLimit(ptr, start, max_end_position);

    if(end_stanza_index == -1)
    {
        Reset(false);
        return kParseInvalidXml;
    }

    size_t end_stanza_pos = static_cast<size_t>(end_stanza_index);

    if(end_stanza_pos == max_end_position)
    {
        if(max_stanza_bytes_ && max_end_position == max_stanza_bytes_)
        {
            Reset(false);
            return kParseStanzaLimitHit;
        }

        if(nested_level_ == -1 && process_root_ == false)
        {
            //finished the stream
            end_stream_handler_(user_data, root_name_);
            Reset(true);
            return kParseOk;
        }

        return kParseOk;
    }

    end_stanza_pos++;

    if(!PushStanza(ptr, end_stanza_pos, user_data))
    {
        Reset(false);
        return kParseInvalidXml;
    }

    buffer_.Consume(end_stanza_pos);

    size_t remaining = buffer_.Length();

    if(!remaining)
        return kParseOk;

    return DoProcess(0, remaining, user_data);
}

int64_t XmlStreamParser::FindStanzaUpperLimit(const uint8_t* ptr, size_t start, size_t end)
{
    size_t index = start;

    if(last_start_tag_index_ == -1)
    {
        while (index < end && kLookupWhitespace[ptr[index]])
            index++;

        if(index < end && ptr[index] != '<')
            return -1;
    }

    for(; index < end; index++)
    {
        switch (ptr[index])
        {
            case '<':

                if(first_start_tag_index_ == -1)
                    first_start_tag_index_ = index;

                last_start_tag_index_ = index;
                break;

            case '>':

                if(last_start_tag_index_ == -1)
                    return -1;

                if(ptr[last_start_tag_index_+1] == '/')
                {
                    nested_level_--;
                }
                else
                {
                    if(kLookupSkipTag[ptr[index - 1]] == 0)
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
    if(process_root_)
        return ProcessRootElement(buffer, length, user_data);

    // don't parse anything in case we have a header or comment as first element
    // for this reason we need to skip all spaces

    if(!kLookupSkipTag[buffer[first_start_tag_index_+1]])
    {
        pugi::xml_parse_status result = pugi_doc_.load_buffer_inplace(buffer, length).status;

        if(result != pugi::status_ok)
            return false;

        element_handler_(user_data, pugi_doc_, strip_invalid_utf8_);
    }

    last_start_tag_index_ = -1;
    first_start_tag_index_ = -1;
    assert(nested_level_ == 0);
    return true;
}

bool XmlStreamParser::ProcessRootElement(uint8_t* buffer, size_t length, void* user_data)
{
    if(!length)
        return false;

    ByteBuffer rootbuff(length+1);
    rootbuff.WriteBytes(buffer, length-1);
    rootbuff.WriteBytes(reinterpret_cast<const uint8_t*>("/>"), 2);

    pugi::xml_parse_status result = pugi_doc_.load_buffer_inplace(const_cast<uint8_t*>(rootbuff.Data()), rootbuff.Length(), pugi::parse_default, pugi::encoding_utf8).status;

    if(result != pugi::status_ok)
        return false;

    if(!start_stream_handler_(user_data, pugi_doc_, strip_invalid_utf8_))
        return false;

    //drop all bytes so far
    root_name_ = pugi_doc_.document_element().name();
    process_root_ = false;
    last_start_tag_index_ = -1;
    first_start_tag_index_ = -1;

    return true;
}

void XmlStreamParser::Reset(bool cleanup)
{
    if(cleanup)
    {
        buffer_.Clear();
        buffer_.Resize(kDefaultBufferSize);
    }

    nested_level_ = -1;
    process_root_ = true;
    last_start_tag_index_ = -1;
    first_start_tag_index_ = -1;
}
