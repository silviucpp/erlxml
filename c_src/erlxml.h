#ifndef ERLXML_C_SRC_ERLXML_H_
#define ERLXML_C_SRC_ERLXML_H_

#include "erl_nif.h"

ERL_NIF_TERM enif_stream_parser_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
void enif_stream_parser_free(ErlNifEnv* env, void* obj);

ERL_NIF_TERM enif_stream_parser_feed(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM enif_stream_parser_reset(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM enif_dom_parse(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#endif
