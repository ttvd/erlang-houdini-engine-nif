#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"


bool hapi_enum_image_data_format_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_ImageDataFormat* image_data_format)
{
    bool nif_success = true;

    uint32_t atom_hash = 0;
    int32_t tuple_size = 0;
    const ERL_NIF_TERM* hash_tuple = NULL;

    if(enif_is_tuple(env, term) && enif_get_tuple(env, term, &tuple_size, &hash_tuple) && (2 == tuple_size))
    {
        if(!enif_get_uint(env, hash_tuple[1], &atom_hash))
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
            }

            // "hapi_image_data_int8"
            case 261911782:
            {
                *image_data_format = HAPI_IMAGE_DATA_INT8;
            }

            // "hapi_image_data_int16"
            case 3832188088:
            {
                *image_data_format = HAPI_IMAGE_DATA_INT16;
            }

            // "hapi_image_data_int32"
            case 2365989835:
            {
                *image_data_format = HAPI_IMAGE_DATA_INT32;
            }

            // "hapi_image_data_float16"
            case 3680643574:
            {
                *image_data_format = HAPI_IMAGE_DATA_FLOAT16;
            }

            // "hapi_image_data_float32"
            case 1193977332:
            {
                *image_data_format = HAPI_IMAGE_DATA_FLOAT32;
            }

            // "hapi_image_data_max"
            case 3576233170:
            {
                *image_data_format = HAPI_IMAGE_DATA_MAX;
            }

            // "hapi_image_data_default"
            case 3085908946:
            {
                *image_data_format = HAPI_IMAGE_DATA_DEFAULT;
            }

            default:
            {
                break;
            }
        }
    }

label_cleanup:

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
