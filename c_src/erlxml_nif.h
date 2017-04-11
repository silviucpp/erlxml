#ifndef ERLXML_C_SRC_ERLXML_NIF_H_
#define ERLXML_C_SRC_ERLXML_NIF_H_

#include "erl_nif.h"

struct atoms
{
    ERL_NIF_TERM atomOk;
    ERL_NIF_TERM atomError;
    ERL_NIF_TERM atomTrue;
    ERL_NIF_TERM atomFalse;
    ERL_NIF_TERM atomBadArg;
    ERL_NIF_TERM atomOptions;

    //errors
    ERL_NIF_TERM atomErrorInvalidStanza;
    ERL_NIF_TERM atomErrorMaxStanzaLimitHit;

    //options
    ERL_NIF_TERM atomStanzaLimit;

    //elements

    ERL_NIF_TERM atomXmlel;
    ERL_NIF_TERM atomXmlcdata;
    ERL_NIF_TERM atomXmlStreamStart;
    ERL_NIF_TERM atomXmlStreamEnd;

};

struct erlxml_data
{
    ErlNifResourceType* res_xml_stream_parser;
};

extern atoms ATOMS;

#endif
