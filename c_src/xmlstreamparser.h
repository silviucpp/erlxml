#ifndef ERLXML_C_SRC_XMLSTREAMPARSER_H_
#define ERLXML_C_SRC_XMLSTREAMPARSER_H_

#include <stdint.h>

#include "pugixml.hpp"
#include "bytebuffer.h"
#include "macros.h"

typedef bool (*XmlStartStreamHandler) (void* user_data, pugi::xml_document& doc);
typedef void (*XmlEndStreamHandler) (void* user_data, const std::string& name);
typedef void (*XmlStreamElementHandler) (void* user_data, pugi::xml_document& doc);

class XmlStreamParser
{
public:

    enum parse_result { kParseOk = 0, kParseStanzaLimitHit, kParseInvalidXml };

    XmlStreamParser(size_t max_stanza, XmlStartStreamHandler start_h, XmlEndStreamHandler end_h, XmlStreamElementHandler el_h);
    ~XmlStreamParser();

    parse_result FeedData(const uint8_t* data, size_t size, void* user_data);
    void Reset();

private:

    bool PushStanza(uint8_t* buffer, size_t length, void* user_data, bool copy);
    size_t FindStanzaUpperLimit(const uint8_t* ptr, size_t size);
    bool ProcessRootElement(uint8_t* buffer, size_t length, void* user_data);

    void Cleanup();

    bool process_root_;
    size_t max_stanza_bytes_;
    ByteBuffer buffer_;
    XmlStartStreamHandler start_stream_handler_;
    XmlEndStreamHandler end_stream_handler_;
    XmlStreamElementHandler element_handler_;
    int32_t nested_level_;
    uint8_t last_char_;
    bool end_begin_detected_;
    pugi::xml_document pugi_doc_;
    std::string root_name_;
};

#endif
