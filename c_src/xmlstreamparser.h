//
//  xmlstreamparser.h
//
//  Created by silviu on 3/23/17.
//  Copyright © 2017 silviu. All rights reserved.
//

#ifndef ERLXML_C_SRC_XMLSTREAMPARSER_H_
#define ERLXML_C_SRC_XMLSTREAMPARSER_H_

#include <stdint.h>

#include "pugixml.hpp"
#include "bytebuffer.h"
#include "macros.h"

typedef void (*XmlStreamElementHandler) (void* user_data, pugi::xml_document& doc);

class XmlStreamParser
{
public:
    
    enum parse_result { kParseOk = 0, kParseStanzaLimitHit, kParseInvalidXml };
    
    XmlStreamParser(bool skip_root, size_t max_stanza, XmlStreamElementHandler handler);
    ~XmlStreamParser();
    
    parse_result FeedData(const uint8_t* data, size_t size, void* user_data);
    void Reset(bool skip_root);
    
private:
    
    bool PushStanza(uint8_t* buffer, size_t length, void* user_data);
    size_t FindStanzaUpperLimit(const uint8_t* ptr, size_t size);
    void Cleanup();

    bool skip_root_;
    size_t max_stanza_bytes_;
    ByteBuffer buffer_;
    XmlStreamElementHandler handler_;
    int32_t nested_level_;
    uint8_t last_char_;
    bool end_begin_detected_;
    pugi::xml_document pugi_doc_;
};

#endif
