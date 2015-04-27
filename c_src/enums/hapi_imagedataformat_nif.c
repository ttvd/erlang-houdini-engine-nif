/// @author Mykola Konyk <mykola@konyk.org>
///
/// @copyright 2015
/// @license MS-RL
/// This file is autogenerated from util/hapi_enum_nif.c.template
/// This file corresponds to HAPI_ImageDataFormat enum from HAPI_Common.h

#include "../hapi_private_nif.h"
#include <string.h>


bool
hapi_make_hapi_imagedataformat_(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_ImageDataFormat* enum_result)
{
    bool nif_success = true;
    uint32_t atom_len = 0u;
    uint32_t atom_hash = 0u;
    char* atom_value = NULL;

    if(!enif_get_atom_length(env, term, &atom_len, ERL_NIF_LATIN1))
    {
        nif_success = false;
        goto label_cleanup;
    }

    atom_value = malloc(atom_len + 1);
    memset(atom_value, 0, atom_len + 1);

    if(!enif_get_atom(env, term, atom_value, atom_len + 1, ERL_NIF_LATIN1))
    {
        nif_success = false;
        goto label_cleanup;
    }

    atom_hash = XXH32(atom_value, strlen(atom_value), 0);

    switch(atom_hash)
    {
        /* hapi_image_data_unknown */
        case 295456066:
        {
            *enum_result = HAPI_IMAGE_DATA_UNKNOWN;
            break;
        }

        /* hapi_image_data_int8 */
        case 261911782:
        {
            *enum_result = HAPI_IMAGE_DATA_INT8;
            break;
        }

        /* hapi_image_data_int16 */
        case 3832188088:
        {
            *enum_result = HAPI_IMAGE_DATA_INT16;
            break;
        }

        /* hapi_image_data_int32 */
        case 2365989835:
        {
            *enum_result = HAPI_IMAGE_DATA_INT32;
            break;
        }

        /* hapi_image_data_float16 */
        case 3680643574:
        {
            *enum_result = HAPI_IMAGE_DATA_FLOAT16;
            break;
        }

        /* hapi_image_data_float32 */
        case 1193977332:
        {
            *enum_result = HAPI_IMAGE_DATA_FLOAT32;
            break;
        }

        /* hapi_image_data_max */
        case 3576233170:
        {
            *enum_result = HAPI_IMAGE_DATA_MAX;
            break;
        }

        /* hapi_image_data_default */
        case 3085908946:
        {
            *enum_result = HAPI_IMAGE_DATA_DEFAULT;
            break;
        }

        default:
        {
            nif_success = false;
            break;
        }
    }

label_cleanup:

    if(atom_value)
    {
        free(atom_value);
    }

    return nif_success;
}


ERL_NIF_TERM
hapi_get_hapi_imagedataformat_c_to_erl(ErlNifEnv* env, HAPI_ImageDataFormat enum_value)
{
    switch(enum_value)
    {
        case HAPI_IMAGE_DATA_UNKNOWN:
        {
            return hapi_make_atom(env, "hapi_image_data_unknown");
        }

        case HAPI_IMAGE_DATA_INT8:
        {
            return hapi_make_atom(env, "hapi_image_data_int8");
        }

        case HAPI_IMAGE_DATA_INT16:
        {
            return hapi_make_atom(env, "hapi_image_data_int16");
        }

        case HAPI_IMAGE_DATA_INT32:
        {
            return hapi_make_atom(env, "hapi_image_data_int32");
        }

        case HAPI_IMAGE_DATA_FLOAT16:
        {
            return hapi_make_atom(env, "hapi_image_data_float16");
        }

        case HAPI_IMAGE_DATA_FLOAT32:
        {
            return hapi_make_atom(env, "hapi_image_data_float32");
        }

        case HAPI_IMAGE_DATA_MAX:
        {
            return hapi_make_atom(env, "hapi_image_data_max");
        }

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
