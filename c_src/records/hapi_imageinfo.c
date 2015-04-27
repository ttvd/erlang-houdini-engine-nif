/// @author Mykola Konyk <mykola@konyk.org>
///
/// @copyright 2015
/// @license MS-RL
/// This file is autogenerated from util/hapi_enum_nif.c.template
/// This file corresponds to HAPI_ImageInfo struct from HAPI_Common.h

#include "../hapi_private_nif.h"
#include "../hapi_records_nif.h"
#include <string.h>


ERL_NIF_TERM
hapi_make_hapi_imageinfo(ErlNifEnv* env, const HAPI_ImageInfo* hapi_struct)
{
    return enif_make_tuple(env, 8,
        hapi_make_atom(env, "hapi_imageinfo"),
        enif_make_int(env, (int32_t) hapi_struct->imageFileFormatNameSH),
        enif_make_int(env, hapi_struct->xRes),
        enif_make_int(env, hapi_struct->yRes),
        enif_make_int(env, (int32_t) hapi_struct->dataFormat),
        hapi_make_atom_bool(env, (bool) hapi_struct->interleaved),
        enif_make_int(env, (int32_t) hapi_struct->packing),
        enif_make_double(env, hapi_struct->gamma));
}


bool
hapi_get_hapi_imageinfo(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_ImageInfo* hapi_struct)
{
    int32_t tuple_size = 0;
    const ERL_NIF_TERM* tuple_record = NULL;
    bool atom_name_match = false;

    int32_t record_image_file_format_name_sh = 0;
    int32_t record_x_res = 0;
    int32_t record_y_res = 0;
    int32_t record_data_format = 0;
    bool record_interleaved = false;
    int32_t record_packing = 0;
    double record_gamma = 0.0;

    if(!enif_get_tuple(env, term, &tuple_size, &tuple_record) ||
        (tuple_size != 8) ||
        !hapi_check_atom(env, tuple_record[0], "hapi_imageinfo", &atom_name_match) ||
        !atom_name_match ||
        !enif_get_int(env, tuple_record[1], &record_image_file_format_name_sh) ||
        !enif_get_int(env, tuple_record[2], &record_x_res) ||
        !enif_get_int(env, tuple_record[3], &record_y_res) ||
        !enif_get_int(env, tuple_record[4], &record_data_format) ||
        !hapi_get_atom_bool(env, tuple_record[5], &record_interleaved) ||
        !enif_get_int(env, tuple_record[6], &record_packing) ||
        !enif_get_double(env, tuple_record[7], &record_gamma))
    {
        return false;
    }

    hapi_struct->imageFileFormatNameSH = (HAPI_StringHandle) record_image_file_format_name_sh;
    hapi_struct->xRes = record_x_res;
    hapi_struct->yRes = record_y_res;
    hapi_struct->dataFormat = (HAPI_ImageDataFormat) record_data_format;
    hapi_struct->interleaved = (HAPI_Bool) record_interleaved;
    hapi_struct->packing = (HAPI_ImagePacking) record_packing;
    hapi_struct->gamma = record_gamma;

    return true;
}
