#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"


bool hapi_enum_image_data_format_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_ImageDataFormat* image_data_format)
{
    bool nif_success = true;

    uint32_t atom_len = 0;
    char* atom_value = NULL;

    if(enif_is_atom(env, term))
    {
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

        if(!strcmp(atom_value, "hapi_image_data_unknown"))
        {
            *image_data_format = HAPI_IMAGE_DATA_UNKNOWN;
        }
        else if(!strcmp(atom_value, "hapi_image_data_int8"))
        {
            *image_data_format = HAPI_IMAGE_DATA_INT8;
        }
        else if(!strcmp(atom_value, "hapi_image_data_int16"))
        {
            *image_data_format = HAPI_IMAGE_DATA_INT16;
        }
        else if(!strcmp(atom_value, "hapi_image_data_int32"))
        {
            *image_data_format = HAPI_IMAGE_DATA_INT32;
        }
        else if(!strcmp(atom_value, "hapi_image_data_float16"))
        {
            *image_data_format = HAPI_IMAGE_DATA_FLOAT16;
        }
        else if(!strcmp(atom_value, "hapi_image_data_float32"))
        {
            *image_data_format = HAPI_IMAGE_DATA_FLOAT32;
        }
        else if(!strcmp(atom_value, "hapi_image_data_max"))
        {
            *image_data_format = HAPI_IMAGE_DATA_MAX;
        }
        else if(!strcmp(atom_value, "hapi_image_data_default"))
        {
            *image_data_format = HAPI_IMAGE_DATA_DEFAULT;
        }
        else
        {
            nif_success = false;
        }
    }
    else
    {
        nif_success = false;
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
            return hapi_private_make_atom(env, "hapi_image_data_unknown");
        }
        */

        case HAPI_IMAGE_DATA_INT8:
        {
            return hapi_private_make_atom(env, "hapi_image_data_int8");
        }

        case HAPI_IMAGE_DATA_INT16:
        {
            return hapi_private_make_atom(env, "hapi_image_data_int16");
        }

        case HAPI_IMAGE_DATA_INT32:
        {
            return hapi_private_make_atom(env, "hapi_image_data_int32");
        }

        case HAPI_IMAGE_DATA_FLOAT16:
        {
            return hapi_private_make_atom(env, "hapi_image_data_float16");
        }

        case HAPI_IMAGE_DATA_FLOAT32:
        {
            return hapi_private_make_atom(env, "hapi_image_data_float32");
        }

        case HAPI_IMAGE_DATA_MAX:
        {
            return hapi_private_make_atom(env, "hapi_image_data_max");
        }

        /*
        case HAPI_IMAGE_DATA_DEFAULT:
        {
            return hapi_private_make_atom(env, "hapi_image_data_default");
        }
        */

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
