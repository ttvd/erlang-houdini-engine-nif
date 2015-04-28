/// @author Mykola Konyk <mykola@konyk.org>
///
/// @copyright 2015
/// @license MS-RL
/// This file is autogenerated from util/hapi_enum_nif.c.template
/// This file corresponds to HAPI_TimelineOptions struct from HAPI_Common.h

#include "../hapi_private_nif.h"
#include "../hapi_records_nif.h"
#include "../hapi_enums_nif.h"
#include <string.h>


ERL_NIF_TERM
hapi_make_hapi_timeline_options(ErlNifEnv* env, const HAPI_TimelineOptions* hapi_struct)
{
    return enif_make_tuple(env, 4,
        hapi_make_atom(env, "hapi_timeline_options"),
        enif_make_double(env, (double) hapi_struct->fps),
        enif_make_double(env, (double) hapi_struct->startTime),
        enif_make_double(env, (double) hapi_struct->endTime));
}


bool
hapi_get_hapi_timeline_options(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_TimelineOptions* hapi_struct)
{
    int32_t tuple_size = 0;
    const ERL_NIF_TERM* tuple_record = NULL;
    bool atom_name_match = false;

    double record_fps = 0.0;
    double record_start_time = 0.0;
    double record_end_time = 0.0;

    if(!enif_get_tuple(env, term, &tuple_size, &tuple_record) ||
        (tuple_size != 4) ||
        !hapi_check_atom(env, tuple_record[0], "hapi_timeline_options", &atom_name_match) ||
        !atom_name_match ||
        !enif_get_double(env, tuple_record[1], &record_fps) ||
        !enif_get_double(env, tuple_record[2], &record_start_time) ||
        !enif_get_double(env, tuple_record[3], &record_end_time))
    {
        return false;
    }

    hapi_struct->fps = (float) record_fps;
    hapi_struct->startTime = (float) record_start_time;
    hapi_struct->endTime = (float) record_end_time;

    return true;
}


ERL_NIF_TERM
hapi_make_hapi_timeline_options_list(ErlNifEnv* env, const HAPI_TimelineOptions* hapi_structs, int32_t list_size)
{
    ERL_NIF_TERM list = enif_make_list(env, 0);

    for(int32_t idx = list_size - 1; idx >= 0; idx--)
    {
        const HAPI_TimelineOptions* hapi_struct = hapi_structs + idx;
        list = enif_make_list_cell(env, hapi_make_hapi_timeline_options(env, hapi_struct), list);
    }

    return list;
}


bool
hapi_get_hapi_timeline_options_list(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_TimelineOptions* hapi_structs, int32_t list_size)
{
    uint32_t read_list_size = 0;
    ERL_NIF_TERM head, tail;

    if(enif_get_list_length(env, term, &read_list_size) && (list_size == read_list_size))
    {
        ERL_NIF_TERM list = term;
        int32_t index = 0;

        while(enif_get_list_cell(env, list, &head, &tail))
        {
            HAPI_TimelineOptions* hapi_struct = hapi_structs + index;

            if(!hapi_get_hapi_timeline_options(env, head, hapi_struct))
            {
                return false;
            }

            index++;
            list = tail;
        }

        return true;
    }

    return false;
}