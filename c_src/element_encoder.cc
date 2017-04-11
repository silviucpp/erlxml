
#include "element_encoder.h"

#include "nif_utils.h"
#include "erlxml_nif.h"

#include <string.h>

static const int kXmlelArity = 4;
static const int kXmlcdataArity = 2;

// all the time we iterate over attributes and childrens in reverse order
// to make sure we don't have to do lists:reverse in erlang

bool pugi2stream_start(ErlNifEnv*env, const pugi::xml_node& node, ERL_NIF_TERM* list)
{
    if(node.type() != pugi::node_element)
        return false;

    ERL_NIF_TERM name = make_binary(env, node.name(), strlen(node.name()));
    ERL_NIF_TERM attrs = enif_make_list(env, 0);

    for (pugi::xml_attribute_iterator ait = node.attributes_end(); ait != node.attributes_begin();)
    {
        --ait;
        ERL_NIF_TERM key = make_binary(env, ait->name(), strlen(ait->name()));
        ERL_NIF_TERM value = make_binary(env, ait->value(), strlen(ait->value()));
        attrs = enif_make_list_cell(env, enif_make_tuple2(env, key, value), attrs);
    }

    ERL_NIF_TERM xmlstreamstart = enif_make_tuple3(env, ATOMS.atomXmlStreamStart, name, attrs);
    *list = enif_make_list_cell(env, xmlstreamstart, *list);

    return true;
}

void pugi2term(ErlNifEnv*env, const pugi::xml_node& node, ERL_NIF_TERM* list)
{
    switch(node.type())
    {
        case pugi::node_element:
        {
            ERL_NIF_TERM name = make_binary(env, node.name(), strlen(node.name()));
            ERL_NIF_TERM attrs = enif_make_list(env, 0);
            ERL_NIF_TERM childrens = enif_make_list(env, 0);

            for (pugi::xml_attribute_iterator ait = node.attributes_end(); ait != node.attributes_begin();)
            {
                --ait;
                ERL_NIF_TERM key = make_binary(env, ait->name(), strlen(ait->name()));
                ERL_NIF_TERM value = make_binary(env, ait->value(), strlen(ait->value()));
                attrs = enif_make_list_cell(env, enif_make_tuple2(env, key, value), attrs);
            }

            for (pugi::xml_node_iterator nit = node.end(); nit != node.begin();)
            {
                --nit;
                pugi2term(env, *nit, &childrens);
            }

            ERL_NIF_TERM xmlel = enif_make_tuple4(env, ATOMS.atomXmlel, name, attrs, childrens);
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

bool parse_attributes(ErlNifEnv* env, ERL_NIF_TERM list, pugi::xml_node& node)
{
    ERL_NIF_TERM head;
    const ERL_NIF_TERM *items;
    int arity;

    while(enif_get_list_cell(env, list, &head, &list))
    {
        if(!enif_get_tuple(env, head, &arity, &items) || arity != 2)
            return false;

        std::string key;
        std::string value;

        if(!get_string(env, items[0], &key) || !get_string(env, items[1], &value))
            return false;

        node.append_attribute(key.c_str()).set_value(value.c_str());
    }

    return true;
}

bool parse_childrens(ErlNifEnv* env, ERL_NIF_TERM list, pugi::xml_node& node)
{
    ERL_NIF_TERM head;

    while(enif_get_list_cell(env, list, &head, &list))
    {
        if(!term2pugi(env, head, node))
            return false;
    }

    return true;
}

bool term2pugi(ErlNifEnv* env, ERL_NIF_TERM element, pugi::xml_node& node)
{
    const ERL_NIF_TERM *items;
    int arity;

    if(!enif_get_tuple(env, element, &arity, &items))
        return false;

    if(arity == kXmlelArity && enif_is_identical(ATOMS.atomXmlel, items[0]))
    {
        //parse xmlel
        std::string name;

        if(!get_string(env, items[1], & name))
            return false;

        pugi::xml_node element = node.append_child(name.c_str());

        if(!parse_attributes(env, items[2], element))
            return false;

        if(!parse_childrens(env, items[3], element))
            return false;
    }
    else if(arity == kXmlcdataArity && enif_is_identical(ATOMS.atomXmlcdata, items[0]))
    {
        std::string value;

        if(!get_string(env, items[1], &value))
            return false;

        node.append_child(pugi::node_pcdata).set_value(value.c_str());
    }
    else
    {
        return false;
    }

    return true;
}
