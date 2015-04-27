/// @author Mykola Konyk <mykola@konyk.org>
///
/// @copyright 2015
/// @license MS-RL
/// This file is autogenerated from util/hapi_enum_nif.c.template
/// This file corresponds to HAPI_ParmInfo struct from HAPI_Common.h

#include "../hapi_private_nif.h"
#include "../hapi_records_nif.h"
#include "../hapi_enums_nif.h"
#include <string.h>


ERL_NIF_TERM
hapi_make_hapi_parminfo(ErlNifEnv* env, const HAPI_ParmInfo* hapi_struct)
{
    return enif_make_tuple(env, 35,
        hapi_make_atom(env, "hapi_parminfo"),
        enif_make_int(env, (int32_t) hapi_struct->id),
        enif_make_int(env, (int32_t) hapi_struct->parentId),
        hapi_parmtype_c_to_erl(env, hapi_struct->type),
        enif_make_int(env, (int32_t) hapi_struct->typeInfoSH),
        hapi_permissions_c_to_erl(env, hapi_struct->permissions),
        enif_make_int(env, hapi_struct->size),
        enif_make_int(env, hapi_struct->choiceCount),
        enif_make_int(env, (int32_t) hapi_struct->nameSH),
        enif_make_int(env, (int32_t) hapi_struct->labelSH),
        enif_make_int(env, (int32_t) hapi_struct->templateNameSH),
        enif_make_int(env, (int32_t) hapi_struct->helpSH),
        hapi_make_atom_bool(env, (bool) hapi_struct->hasMin),
        hapi_make_atom_bool(env, (bool) hapi_struct->hasMax),
        hapi_make_atom_bool(env, (bool) hapi_struct->hasUIMin),
        hapi_make_atom_bool(env, (bool) hapi_struct->hasUIMax),
        enif_make_double(env, (double) hapi_struct->min),
        enif_make_double(env, (double) hapi_struct->max),
        enif_make_double(env, (double) hapi_struct->UIMin),
        enif_make_double(env, (double) hapi_struct->UIMax),
        hapi_make_atom_bool(env, (bool) hapi_struct->invisible),
        hapi_make_atom_bool(env, (bool) hapi_struct->disabled),
        hapi_make_atom_bool(env, (bool) hapi_struct->spare),
        hapi_make_atom_bool(env, (bool) hapi_struct->joinNext),
        hapi_make_atom_bool(env, (bool) hapi_struct->labelNone),
        enif_make_int(env, hapi_struct->intValuesIndex),
        enif_make_int(env, hapi_struct->floatValuesIndex),
        enif_make_int(env, hapi_struct->stringValuesIndex),
        enif_make_int(env, hapi_struct->choiceIndex),
        hapi_make_atom_bool(env, (bool) hapi_struct->isChildOfMultiParm),
        enif_make_int(env, hapi_struct->instanceNum),
        enif_make_int(env, hapi_struct->instanceLength),
        enif_make_int(env, hapi_struct->instanceCount),
        enif_make_int(env, hapi_struct->instanceStartOffset),
        hapi_ramptype_c_to_erl(env, hapi_struct->rampType));
}


bool
hapi_get_hapi_parminfo(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_ParmInfo* hapi_struct)
{
    int32_t tuple_size = 0;
    const ERL_NIF_TERM* tuple_record = NULL;
    bool atom_name_match = false;

    int32_t record_id = 0;
    int32_t record_parent_id = 0;
    HAPI_ParmType record_type;
    int32_t record_type_info_sh = 0;
    HAPI_Permissions record_permissions;
    int32_t record_size = 0;
    int32_t record_choice_count = 0;
    int32_t record_name_sh = 0;
    int32_t record_label_sh = 0;
    int32_t record_template_name_sh = 0;
    int32_t record_help_sh = 0;
    bool record_has_min = false;
    bool record_has_max = false;
    bool record_has_uimin = false;
    bool record_has_uimax = false;
    double record_min = 0.0;
    double record_max = 0.0;
    double record_uimin = 0.0;
    double record_uimax = 0.0;
    bool record_invisible = false;
    bool record_disabled = false;
    bool record_spare = false;
    bool record_join_next = false;
    bool record_label_none = false;
    int32_t record_int_values_index = 0;
    int32_t record_float_values_index = 0;
    int32_t record_string_values_index = 0;
    int32_t record_choice_index = 0;
    bool record_is_child_of_multi_parm = false;
    int32_t record_instance_num = 0;
    int32_t record_instance_length = 0;
    int32_t record_instance_count = 0;
    int32_t record_instance_start_offset = 0;
    HAPI_RampType record_ramp_type;

    if(!enif_get_tuple(env, term, &tuple_size, &tuple_record) ||
        (tuple_size != 35) ||
        !hapi_check_atom(env, tuple_record[0], "hapi_parminfo", &atom_name_match) ||
        !atom_name_match ||
        !enif_get_int(env, tuple_record[1], &record_id) ||
        !enif_get_int(env, tuple_record[2], &record_parent_id) ||
        !hapi_parmtype_erl_to_c(env, tuple_record[3], &record_type) ||
        !enif_get_int(env, tuple_record[4], &record_type_info_sh) ||
        !hapi_permissions_erl_to_c(env, tuple_record[5], &record_permissions) ||
        !enif_get_int(env, tuple_record[6], &record_size) ||
        !enif_get_int(env, tuple_record[7], &record_choice_count) ||
        !enif_get_int(env, tuple_record[8], &record_name_sh) ||
        !enif_get_int(env, tuple_record[9], &record_label_sh) ||
        !enif_get_int(env, tuple_record[10], &record_template_name_sh) ||
        !enif_get_int(env, tuple_record[11], &record_help_sh) ||
        !hapi_get_atom_bool(env, tuple_record[12], &record_has_min) ||
        !hapi_get_atom_bool(env, tuple_record[13], &record_has_max) ||
        !hapi_get_atom_bool(env, tuple_record[14], &record_has_uimin) ||
        !hapi_get_atom_bool(env, tuple_record[15], &record_has_uimax) ||
        !enif_get_double(env, tuple_record[16], &record_min) ||
        !enif_get_double(env, tuple_record[17], &record_max) ||
        !enif_get_double(env, tuple_record[18], &record_uimin) ||
        !enif_get_double(env, tuple_record[19], &record_uimax) ||
        !hapi_get_atom_bool(env, tuple_record[20], &record_invisible) ||
        !hapi_get_atom_bool(env, tuple_record[21], &record_disabled) ||
        !hapi_get_atom_bool(env, tuple_record[22], &record_spare) ||
        !hapi_get_atom_bool(env, tuple_record[23], &record_join_next) ||
        !hapi_get_atom_bool(env, tuple_record[24], &record_label_none) ||
        !enif_get_int(env, tuple_record[25], &record_int_values_index) ||
        !enif_get_int(env, tuple_record[26], &record_float_values_index) ||
        !enif_get_int(env, tuple_record[27], &record_string_values_index) ||
        !enif_get_int(env, tuple_record[28], &record_choice_index) ||
        !hapi_get_atom_bool(env, tuple_record[29], &record_is_child_of_multi_parm) ||
        !enif_get_int(env, tuple_record[30], &record_instance_num) ||
        !enif_get_int(env, tuple_record[31], &record_instance_length) ||
        !enif_get_int(env, tuple_record[32], &record_instance_count) ||
        !enif_get_int(env, tuple_record[33], &record_instance_start_offset) ||
        !hapi_ramptype_erl_to_c(env, tuple_record[34], &record_ramp_type))
    {
        return false;
    }

    hapi_struct->id = (HAPI_ParmId) record_id;
    hapi_struct->parentId = (HAPI_ParmId) record_parent_id;
    hapi_struct->type = record_type;
    hapi_struct->typeInfoSH = (HAPI_StringHandle) record_type_info_sh;
    hapi_struct->permissions = record_permissions;
    hapi_struct->size = record_size;
    hapi_struct->choiceCount = record_choice_count;
    hapi_struct->nameSH = (HAPI_StringHandle) record_name_sh;
    hapi_struct->labelSH = (HAPI_StringHandle) record_label_sh;
    hapi_struct->templateNameSH = (HAPI_StringHandle) record_template_name_sh;
    hapi_struct->helpSH = (HAPI_StringHandle) record_help_sh;
    hapi_struct->hasMin = (HAPI_Bool) record_has_min;
    hapi_struct->hasMax = (HAPI_Bool) record_has_max;
    hapi_struct->hasUIMin = (HAPI_Bool) record_has_uimin;
    hapi_struct->hasUIMax = (HAPI_Bool) record_has_uimax;
    hapi_struct->min = (float) record_min;
    hapi_struct->max = (float) record_max;
    hapi_struct->UIMin = (float) record_uimin;
    hapi_struct->UIMax = (float) record_uimax;
    hapi_struct->invisible = (HAPI_Bool) record_invisible;
    hapi_struct->disabled = (HAPI_Bool) record_disabled;
    hapi_struct->spare = (HAPI_Bool) record_spare;
    hapi_struct->joinNext = (HAPI_Bool) record_join_next;
    hapi_struct->labelNone = (HAPI_Bool) record_label_none;
    hapi_struct->intValuesIndex = record_int_values_index;
    hapi_struct->floatValuesIndex = record_float_values_index;
    hapi_struct->stringValuesIndex = record_string_values_index;
    hapi_struct->choiceIndex = record_choice_index;
    hapi_struct->isChildOfMultiParm = (HAPI_Bool) record_is_child_of_multi_parm;
    hapi_struct->instanceNum = record_instance_num;
    hapi_struct->instanceLength = record_instance_length;
    hapi_struct->instanceCount = record_instance_count;
    hapi_struct->instanceStartOffset = record_instance_start_offset;
    hapi_struct->rampType = record_ramp_type;

    return true;
}