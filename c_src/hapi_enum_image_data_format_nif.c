#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"

#include <string.h>


bool hapi_enum_image_data_format_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_ImageDataFormat* image_data_format)
{
    bool nif_success = true;

    uint32_t atom_hash = 0;
    int32_t tuple_size = 0;
    const ERL_NIF_TERM* hash_tuple = NULL;

    char* atom_value = NULL;

    if(enif_is_tuple(env, term) && enif_get_tuple(env, term, &tuple_size, &hash_tuple) && (2 == tuple_size))
    {
        if(!enif_get_uint(env, hash_tuple[1], &atom_hash))
        {
            nif_success = false;
            goto label_cleanup;
        }
    }
    else if(enif_is_atom(env, term))
    {
        uint32_t atom_len = 0;

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
    }
    else if(!enif_get_uint(env, term, &atom_hash))
    {
        nif_success = false;
        goto label_cleanup;
    }

    switch(atom_hash)
    {
        // "hapi_image_data_unknown"
        case 295456066:
        {
            *image_data_format = HAPI_IMAGE_DATA_UNKNOWN;
            break;
        }

        // "hapi_image_data_int8"
        case 261911782:
        {
            *image_data_format = HAPI_IMAGE_DATA_INT8;
            break;
        }

        // "hapi_image_data_int16"
        case 3832188088:
        {
            *image_data_format = HAPI_IMAGE_DATA_INT16;
            break;
        }

        // "hapi_image_data_int32"
        case 2365989835:
        {
            *image_data_format = HAPI_IMAGE_DATA_INT32;
            break;
        }

        // "hapi_image_data_float16"
        case 3680643574:
        {
            *image_data_format = HAPI_IMAGE_DATA_FLOAT16;
            break;
        }

        // "hapi_image_data_float32"
        case 1193977332:
        {
            *image_data_format = HAPI_IMAGE_DATA_FLOAT32;
            break;
        }

        // "hapi_image_data_max"
        case 3576233170:
        {
            *image_data_format = HAPI_IMAGE_DATA_MAX;
            break;
        }

        // "hapi_image_data_default"
        case 3085908946:
        {
            *image_data_format = HAPI_IMAGE_DATA_DEFAULT;
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


ERL_NIF_TERM hapi_enum_image_data_format_c_to_erl(ErlNifEnv* env, HAPI_ImageDataFormat image_data_format)
{
    switch(image_data_format)
    {
        /*
        case HAPI_IMAGE_DATA_UNKNOWN:
        {
            return hapi_private_make_hash_tuple(env, "hapi_image_data_unknown");
        }
        */

        case HAPI_IMAGE_DATA_INT8:
        {
            return hapi_private_make_hash_tuple(env, "hapi_image_data_int8");
        }

        case HAPI_IMAGE_DATA_INT16:
        {
            return hapi_private_make_hash_tuple(env, "hapi_image_data_int16");
        }

        case HAPI_IMAGE_DATA_INT32:
        {
            return hapi_private_make_hash_tuple(env, "hapi_image_data_int32");
        }

        case HAPI_IMAGE_DATA_FLOAT16:
        {
            return hapi_private_make_hash_tuple(env, "hapi_image_data_float16");
        }

        case HAPI_IMAGE_DATA_FLOAT32:
        {
            return hapi_private_make_hash_tuple(env, "hapi_image_data_float32");
        }

        case HAPI_IMAGE_DATA_MAX:
        {
            return hapi_private_make_hash_tuple(env, "hapi_image_data_max");
        }

        /*
        case HAPI_IMAGE_DATA_DEFAULT:
        {
            return hapi_private_make_hash_tuple(env, "hapi_image_data_default");
        }
        */

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
