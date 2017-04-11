#include "erlxml_nif.h"
#include "nif_utils.h"
#include "macros.h"
#include "pugixml.hpp"
#include "allocators.h"
#include "erlxml.h"

const char kAtomOk[] = "ok";
const char kAtomError[] = "error";
const char kAtomTrue[] = "true";
const char kAtomFalse[] = "false";
const char kAtomBadArg[] = "badarg";
const char kAtomOptions[] = "options";

const char kAtomStanzaLimit[] = "stanza_limit";

const char kAtomErrorInvalidStanza[] = "invalid_stanza";
const char kAtomErrorMaxStanzaLimitHit[] = "max_stanza_limit_hit";

const char kAtomXmlel[] = "xmlel";
const char kAtomXmlcdata[] = "xmlcdata";
const char kAtomXmlStreamStart[] = "xmlstreamstart";
const char kAtomXmlStreamEnd[] = "xmlstreamend";

atoms ATOMS;

void open_resources(ErlNifEnv* env, erlxml_data* data)
{
    ErlNifResourceFlags flags =  static_cast<ErlNifResourceFlags>(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
    data->res_xml_stream_parser = enif_open_resource_type(env, NULL, "res_xml_stream_parser", enif_stream_parser_free, flags, NULL);
}

int on_nif_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    UNUSED(load_info);

    pugi::set_memory_management_functions(erlxml_allocate, erlxml_deallocate);

    ATOMS.atomOk = make_atom(env, kAtomOk);
    ATOMS.atomError = make_atom(env, kAtomError);
    ATOMS.atomTrue = make_atom(env, kAtomTrue);
    ATOMS.atomFalse = make_atom(env, kAtomFalse);
    ATOMS.atomOptions = make_atom(env, kAtomOptions);
    ATOMS.atomBadArg = make_atom(env, kAtomBadArg);

    ATOMS.atomErrorInvalidStanza = make_atom(env, kAtomErrorInvalidStanza);
    ATOMS.atomErrorMaxStanzaLimitHit = make_atom(env, kAtomErrorMaxStanzaLimitHit);

    ATOMS.atomStanzaLimit = make_atom(env, kAtomStanzaLimit);

    ATOMS.atomXmlel = make_atom(env, kAtomXmlel);
    ATOMS.atomXmlcdata = make_atom(env, kAtomXmlcdata);
    ATOMS.atomXmlStreamStart = make_atom(env, kAtomXmlStreamStart);
    ATOMS.atomXmlStreamEnd = make_atom(env, kAtomXmlStreamEnd);

    erlxml_data* data = static_cast<erlxml_data*>(enif_alloc(sizeof(erlxml_data)));
    open_resources(env, data);

    *priv_data = data;
    return 0;
}

void on_nif_unload(ErlNifEnv* env, void* priv_data)
{
    UNUSED(env);

    erlxml_data* data = static_cast<erlxml_data*>(priv_data);
    enif_free(data);
}

int on_nif_upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM info)
{
    UNUSED(old_priv);
    UNUSED(info);

    erlxml_data* data = static_cast<erlxml_data*>(enif_alloc(sizeof(erlxml_data)));
    open_resources(env, data);

    *priv = data;
    return 0;
}

static ErlNifFunc nif_funcs[] =
{
    {"new_stream", 1, enif_stream_parser_new},
    {"feed_stream", 2, enif_stream_parser_feed},
    {"reset_stream", 1, enif_stream_parser_reset},
    {"dom_parse", 1, enif_dom_parse},
    {"to_binary", 1, enif_dom_to_binary}
};

ERL_NIF_INIT(erlxml_nif, nif_funcs, on_nif_load, NULL, on_nif_upgrade, on_nif_unload)
