#include "nif_utils.h"
#include "erlxml_nif.h"
#include "macros.h"

#include <string.h>

// This should correspond to the similar define in erlxml_nif.erl
#define MAX_BYTES_TO_NIF 20000

ERL_NIF_TERM make_atom(ErlNifEnv* env, const char* name)
{
    ERL_NIF_TERM ret;

    if(enif_make_existing_atom(env, name, &ret, ERL_NIF_LATIN1))
        return ret;

    return enif_make_atom(env, name);
}

ERL_NIF_TERM make_binary(ErlNifEnv* env, const char* buff, size_t length)
{
    ERL_NIF_TERM term;
    uint8_t *destination_buffer = enif_make_new_binary(env, length, &term);
    memcpy(destination_buffer, buff, length);
    return term;
}

ERL_NIF_TERM make_error(ErlNifEnv* env, const char* error)
{
    return make_error(env, make_binary(env, error, strlen(error)));
}

ERL_NIF_TERM make_error(ErlNifEnv* env, ERL_NIF_TERM term)
{
    return enif_make_tuple2(env, ATOMS.atomError, term);
}

ERL_NIF_TERM make_bad_options(ErlNifEnv* env, ERL_NIF_TERM term)
{
    return make_error(env, enif_make_tuple(env, 2, ATOMS.atomOptions, term));
}

ERL_NIF_TERM make_badarg(ErlNifEnv* env)
{
    return enif_make_tuple2(env, ATOMS.atomError, ATOMS.atomBadArg);
}

ERL_NIF_TERM make_ok_result(ErlNifEnv* env, ERL_NIF_TERM term)
{
    return enif_make_tuple(env, 2, ATOMS.atomOk, term);
}

void consume_timeslice(ErlNifEnv *env, size_t bytes)
{
    int cost = static_cast<int>((bytes * 100) / MAX_BYTES_TO_NIF);

    if(cost)
        enif_consume_timeslice(env, cost > 100 ? 100 : cost);
}

bool get_binary(ErlNifEnv* env, ERL_NIF_TERM term, ErlNifBinary* bin)
{
    if(enif_is_binary(env, term))
        return enif_inspect_binary(env, term, bin);

    return enif_inspect_iolist_as_binary(env, term, bin);
}

bool get_boolean(ERL_NIF_TERM term, bool* val)
{
    if(enif_is_identical(term, ATOMS.atomTrue))
    {
        *val = true;
        return true;
    }

    if(enif_is_identical(term, ATOMS.atomFalse))
    {
        *val = false;
        return true;
    }

    return false;
}
