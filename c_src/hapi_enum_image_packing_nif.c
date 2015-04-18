#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"

#include <string.h>


bool hapi_enum_image_packing_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_ImagePacking* image_packing)
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
        // "hapi_image_packing_unknown"
        case 1026533579:
        {
            *image_packing = HAPI_IMAGE_PACKING_UNKNOWN;
            break;
        }

        // "hapi_image_packing_single"
        case 4027772177:
        {
            *image_packing = HAPI_IMAGE_PACKING_SINGLE;
            break;
        }

        // "hapi_image_packing_dual"
        case 3618580164:
        {
            *image_packing = HAPI_IMAGE_PACKING_DUAL;
            break;
        }

        // "hapi_image_packing_rgb"
        case 1954788252:
        {
            *image_packing = HAPI_IMAGE_PACKING_RGB;
            break;
        }

        // "hapi_image_packing_bgr"
        case 3355295031:
        {
            *image_packing = HAPI_IMAGE_PACKING_BGR;
            break;
        }

        // "hapi_image_packing_rgba"
        case 211676614:
        {
            *image_packing = HAPI_IMAGE_PACKING_RGBA;
            break;
        }

        // "hapi_image_packing_abgr"
        case 4154788832:
        {
            *image_packing = HAPI_IMAGE_PACKING_ABGR;
            break;
        }

        // "hapi_image_packing_max"
        case 2046891834:
        {
            *image_packing = HAPI_IMAGE_PACKING_MAX;
            break;
        }

        // "hapi_image_packing_default3"
        case 3652598404:
        {
            *image_packing = HAPI_IMAGE_PACKING_DEFAULT3;
            break;
        }

        // "hapi_image_packing_default4"
        case 1347420588:
        {
            *image_packing = HAPI_IMAGE_PACKING_DEFAULT4;
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


ERL_NIF_TERM hapi_enum_image_packing_c_to_erl(ErlNifEnv* env, HAPI_ImagePacking image_packing)
{
    switch(image_packing)
    {
        /*
        case HAPI_IMAGE_PACKING_UNKNOWN:
        {
            return hapi_private_make_hash_tuple(env, "hapi_image_packing_unknown");
        }
        */

        case HAPI_IMAGE_PACKING_SINGLE:
        {
            return hapi_private_make_hash_tuple(env, "hapi_image_packing_single");
        }

        case HAPI_IMAGE_PACKING_DUAL:
        {
            return hapi_private_make_hash_tuple(env, "hapi_image_packing_dual");
        }

        case HAPI_IMAGE_PACKING_RGB:
        {
            return hapi_private_make_hash_tuple(env, "hapi_image_packing_rgb");
        }

        case HAPI_IMAGE_PACKING_BGR:
        {
            return hapi_private_make_hash_tuple(env, "hapi_image_packing_bgr");
        }

        case HAPI_IMAGE_PACKING_RGBA:
        {
            return hapi_private_make_hash_tuple(env, "hapi_image_packing_rgba");
        }

        case HAPI_IMAGE_PACKING_ABGR:
        {
            return hapi_private_make_hash_tuple(env, "hapi_image_packing_abgr");
        }

        case HAPI_IMAGE_PACKING_MAX:
        {
            return hapi_private_make_hash_tuple(env, "hapi_image_packing_max");
        }

        /*
        case HAPI_IMAGE_PACKING_DEFAULT3:
        {
            return hapi_private_make_hash_tuple(env, "hapi_image_packing_default3");
        }
        */

        /*
        case HAPI_IMAGE_PACKING_DEFAULT4:
        {
            return hapi_private_make_hash_tuple(env, "hapi_image_packing_default4");
        }
        */

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
