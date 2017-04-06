#include "erlxml.h"
#include "erlxml_nif.h"
#include "nif_utils.h"
#include "xmlstreamparser.h"

static const char kErrorFailedToAllocXmlStream[] = "failed to alloc stream object";
static const char kErrorBadOwner[] = "erlxml session was created on a different process";

struct enif_erlxml_stream
{
    XmlStreamParser* parser;
    ERL_NIF_TERM owner_pid;
};

struct stream_options
{
    stream_options() : skip_root(true), stanza_limit(0) {}
    
    bool skip_root;
    size_t stanza_limit;
};

struct parser_data
{
    parser_data(ErlNifEnv* e, ERL_NIF_TERM t) : env(e), term(t) {}

    ErlNifEnv* env;
    ERL_NIF_TERM term;
};

void enif_stream_parser_free(ErlNifEnv* env, void* obj)
{
    UNUSED(env);
    
    enif_erlxml_stream* stream = static_cast<enif_erlxml_stream*>(obj);
    
    if(stream->parser != NULL)
        delete stream->parser;
}

void node_walker(ErlNifEnv*env, const pugi::xml_node& node, ERL_NIF_TERM* list)
{
    switch(node.type())
    {
        case pugi::node_element:
        {
            ERL_NIF_TERM name = make_binary(env, node.name(), strlen(node.name()));
            ERL_NIF_TERM attrs = enif_make_list(env, 0);
            ERL_NIF_TERM childrens = enif_make_list(env, 0);
            
            for (pugi::xml_attribute attr : node.attributes())
            {
                ERL_NIF_TERM key = make_binary(env, attr.name(), strlen(attr.name()));
                ERL_NIF_TERM value = make_binary(env, attr.value(), strlen(attr.value()));
                attrs = enif_make_list_cell(env, enif_make_tuple2(env, key, value), attrs);
            }
            
            for (pugi::xml_node child: node.children())
                node_walker(env, child, &childrens);
            
            ERL_NIF_TERM childrens_reverse;
            enif_make_reverse_list(env, childrens, &childrens_reverse);
            
            ERL_NIF_TERM xmlel = enif_make_tuple4(env, ATOMS.atomXmlel, name, attrs, childrens_reverse);
            *list = enif_make_list_cell(env, xmlel, *list);
            break;
        }
        
        case pugi::node_pcdata:
        {
            ERL_NIF_TERM value = make_binary(env, node.value(), strlen(node.value()));
            *list = enif_make_list_cell(env, enif_make_tuple2(env, ATOMS.atomXmlcdata, value), *list);
            break;
        }
            
        default:;
    }
}

void handle_stanza(void* user_data, pugi::xml_document& doc)
{
    parser_data* wp = reinterpret_cast<parser_data*>(user_data);
    node_walker(wp->env, doc.first_child(), &wp->term);
}

ERL_NIF_TERM parse_stream_options(ErlNifEnv* env, ERL_NIF_TERM list, stream_options* opts)
{
    if(!enif_is_list(env, list))
        return make_bad_options(env, list);
    
    ERL_NIF_TERM head;
    const ERL_NIF_TERM *items;
    int arity;
    
    while(enif_get_list_cell(env, list, &head, &list))
    {
        if(!enif_get_tuple(env, head, &arity, &items) || arity != 2)
            return make_bad_options(env, head);
        
        ERL_NIF_TERM key = items[0];
        ERL_NIF_TERM value = items[1];
        
        if(enif_is_identical(key, ATOMS.atomStanzaLimit))
        {
            if(!get_boolean(value, &opts->skip_root))
                return make_bad_options(env, head);
        }
        else if(enif_is_identical(key, ATOMS.atomStanzaLimit))
        {
            if(!enif_get_uint64(env, value, &opts->stanza_limit))
                return make_bad_options(env, head);
        }
        else
        {
            return make_bad_options(env, head);
        }
    }
    
    return ATOMS.atomOk;
}

ERL_NIF_TERM enif_stream_parser_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    
    erlxml_data* data = static_cast<erlxml_data*>(enif_priv_data(env));
    
    stream_options opts;
    
    ERL_NIF_TERM parse_result = parse_stream_options(env, argv[0], &opts);
    
    if(!enif_is_identical(parse_result, ATOMS.atomOk))
        return parse_result;
    
    enif_erlxml_stream* nif_stream = static_cast<enif_erlxml_stream*>(enif_alloc_resource(data->res_xml_stream_parser, sizeof(enif_erlxml_stream)));
    
    if(nif_stream == NULL)
        return make_error(env, kErrorFailedToAllocXmlStream);
    
    ErlNifPid current_pid;
    enif_self(env, &current_pid);

    nif_stream->parser = new XmlStreamParser(opts.skip_root, opts.stanza_limit, handle_stanza);
    nif_stream->owner_pid = enif_make_pid(env, &current_pid);
    
    ERL_NIF_TERM term = enif_make_resource(env, nif_stream);
    enif_release_resource(nif_stream);
    return enif_make_tuple2(env, ATOMS.atomOk, term);
}

ERL_NIF_TERM enif_stream_parser_feed(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    
    erlxml_data* data = static_cast<erlxml_data*>(enif_priv_data(env));
    
    enif_erlxml_stream* stream = NULL;
    ErlNifBinary bin;
    
    if(!enif_get_resource(env, argv[0], data->res_xml_stream_parser, (void**) &stream))
        return make_badarg(env);
    
    if(!get_binary(env, argv[1], &bin))
        return make_badarg(env);
    
    ErlNifPid current_pid;
    
    if(enif_self(env, &current_pid) && !enif_is_identical(stream->owner_pid, enif_make_pid(env, &current_pid)))
        return make_error(env, kErrorBadOwner);
    
    parser_data parser_data(env, enif_make_list(env, 0));
    XmlStreamParser::parse_result result = stream->parser->FeedData(bin.data, bin.size, &parser_data);
    
    consume_timeslice(env, bin.size);
    
    switch (result)
    {
        case XmlStreamParser::kParseOk:
                return make_ok_result(env, parser_data.term);
        
        case XmlStreamParser::kParseInvalidXml:
            return make_error(env, ATOMS.atomErrorInvalidStanza);
        
        case XmlStreamParser::kParseStanzaLimitHit:
            return make_error(env, ATOMS.atomErrorMaxStanzaLimitHit);
            
        default:
            return make_error(env, "unknown error");
    }
}

ERL_NIF_TERM enif_stream_parser_reset(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    
    erlxml_data* data = static_cast<erlxml_data*>(enif_priv_data(env));
    
    enif_erlxml_stream* stream = NULL;
    ErlNifPid current_pid;
    
    if(!enif_get_resource(env, argv[0], data->res_xml_stream_parser, (void**) &stream))
        return make_badarg(env);
    
    if(enif_self(env, &current_pid) && !enif_is_identical(stream->owner_pid, enif_make_pid(env, &current_pid)))
        return make_error(env, kErrorBadOwner);
    
    bool skip_root;
    
    if(!get_boolean(argv[1], &skip_root))
        return make_badarg(env);
    
    stream->parser->Reset(skip_root);
    return ATOMS.atomOk;
}

ERL_NIF_TERM enif_dom_parse(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    
    ErlNifBinary bin;
    
    if(!get_binary(env, argv[0], &bin))
        return make_badarg(env);
    
    pugi::xml_document pugi_doc;
    
    if(pugi_doc.load_buffer_inplace(bin.data, bin.size).status != pugi::status_ok)
        return make_error(env, ATOMS.atomErrorInvalidStanza);
    
    ERL_NIF_TERM list = enif_make_list(env, 0);
    node_walker(env, pugi_doc.first_child(), &list);
    
    ERL_NIF_TERM head;
    ERL_NIF_TERM tail;
    
    if(!enif_get_list_cell(env, list, &head, &tail))
        return make_error(env, ATOMS.atomErrorInvalidStanza);
    
    return make_ok_result(env, head);
}

