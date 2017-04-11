#ifndef ERLXML_C_SRC_ELEMENTS_ENCODER_H_
#define ERLXML_C_SRC_ELEMENTS_ENCODER_H_

#include "pugixml.hpp"
#include "erl_nif.h"

bool pugi2stream_start(ErlNifEnv*env, const pugi::xml_node& node, ERL_NIF_TERM* list);
void pugi2term(ErlNifEnv*env, const pugi::xml_node& node, ERL_NIF_TERM* list);
bool term2pugi(ErlNifEnv* env, ERL_NIF_TERM element, pugi::xml_node& node);

#endif

