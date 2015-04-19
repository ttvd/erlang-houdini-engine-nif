# Erlang NIF bindings for Houdini Engine.


## Goals of this project.

* To be able to run [Houdini Engine](http://www.sidefx.com/index.php?option=com_content&task=blogcategory&id=227&Itemid=381) instance (and be able to perform asset instantiation, cooking, etc) on an [Erlang](http://www.erlang.org/) (as well as [Elixir](http://elixir-lang.org)) node.
* Create an HTTP RESTful service based on this NIF.
* Create a WebGL client to interact with the RESTful service.

## Before building.

* Install latest build of Houdini 14.
  * If you choose to install to custom location, you may need to patch rebar.config .
  * Install [Erlang](http://www.erlang.org/download.html) (build it yourself or install pre-built).
  * This NIF library relies on enif_schedule_nif, which requires a fairly recent Erlang OTP, for example OTP 17.5 .
  * If you choose to install to a custom location, you may need to patch rebar.config .
* Install [Rebar](https://github.com/rebar/rebar) (for example through [brew](http://brew.sh/)).

## Building.
Use Rebar:

```
rebar get-deps
rebar compile
```

Or use Rake (also run rake without arguments to see available tasks):

```
$ rake erlang:deps
$ rake erlang:compile
```

## Running.  

```
> code:add_path("ebin").
true

> hapi:is_initialized().
{hapi_result_not_initialized,2669115710}

> hapi:initialize(nil, nil, nil, false, -1).
{hapi_result_success,3713831785}

> hapi:is_initialized().
{hapi_result_success,3713831785}

> hapi:cleanup().
{hapi_result_success,3713831785}

> hapi:is_initialized().
{hapi_result_not_initialized,2669115710}

> hapi:initialize("", "", {hapi_cook_options, false, 3, false, 8.0, false, false}, false, -1).
{hapi_result_success,3713831785}

> hapi:is_initialized().
{hapi_result_success,3713831785}

> % pass an atom as parameter.
> hapi:get_env_int(hapi_envint_version_houdini_major).
{{hapi_result_success,3713831785},14}

> % pass a hash of an atom as parameter.
> hapi:get_env_int(729783216).
{{hapi_result_success,3713831785},14}

> % pass a tuple of an atom and a hash as parameter.
> hapi:get_env_int({hapi_envint_version_houdini_major, 729783216}).
{{hapi_result_success,3713831785},14}

> hapi:get_env_int(hapi_envint_version_houdini_minor).
{{hapi_result_success,3713831785},5}

> hapi:get_env_int(hapi_envint_version_houdini_build).
{{hapi_result_success,3713831785},80}

> hapi:load_asset_library_from_file("/Users/radix/HoudiniAssets/nonexistant.hda",true).
{hapi_result_cant_loadfile,2751182470}

> hapi:load_asset_library_from_file("otls/dummyboxes.otl",true).
{{hapi_result_success,3713831785},0}

> hapi:get_available_asset_count(0).
{{hapi_result_success,3713831785},1}

> hapi:get_available_assets(0, 1).
{{hapi_result_success,3713831785},[2982]}

> hapi:get_string_buf_length(2982).
{{hapi_result_success,3713831785},29}

> hapi:get_string(2982, 29).
{{hapi_result_success,3713831785}, "hapi::Object/dummyboxes::2.0"}

> hapi:instantiate_asset("hapi::Object/dummyboxes::2.0", true).
{{hapi_result_success,3713831785},0}

> hapi:destroy_asset(0).
{hapi_result_success,3713831785}

> hapi:get_asset_info(0).
{{hapi_result_success,3713831785},
 {hapi_asset_info,0,0,0,1409326048,3,4,false,2968,2984,2932,2985,2941,2940,4,1,1,1,false,false}}

> hapi:get_node_info(3).
{{hapi_result_success,3713831785},
 {hapi_node_info,3,0,3616,0,35,3635,84,64,60,21,66}}

> hapi:get_parameters(3, 0, 84).
{{hapi_result_success,3713831785},
 [{{hapi_parm_info,81,-1,4,0,0,1,0,3712,3713,3715,3714,3715,
                  3714,false,false,true,true,0.0,10.0,0.0,10.0,false,false,...},
  hapi_parm_info,82,-1,0,0,0,1,6,3767,3768,3770,3769,3770,
                  3769,false,false,true,true,0.0,1.0,0.0,1.0,false,false,...},
  {hapi_parm_info,83,-1,0,0,0,1,6,3786,3787,3789,3788,3789,
                  3788,false,false,true,true,0.0,1.0,0.0,1.0,false,false,...},
...
  {...}|...]}

> hapi:get_parm_info(3, 0).
{{hapi_result_success,3713831785},
 {hapi_parm_info,0,-1,0,0,0,1,0,3052,3053,3055,3054,3055,
               3054,false,false,true,true,0.0,10.0,0.0,10.0,false,false,
               false,false,false,...}}

> hapi:get_parm_info(3, 83).
{{hapi_result_success,3713831785},
 {hapi_parm_info,83,-1,0,0,0,1,6,3786,3787,3789,3788,3789,
                3788,false,false,true,true,0.0,1.0,0.0,1.0,false,false,
                false,false,true,...}}

> hapi:get_parm_int_value(3, "int_1", 0).
{{hapi_result_success,3713831785},1}

> hapi:get_parm_float_value(3, "t", 0).
{{hapi_result_success,3713831785},0.0}

> hapi:cook_asset(0, {hapi_cook_options, false, 3, false, 8.0, false, false}).
{hapi_result_success,3713831785}

> hapi:get_asset_transform(0, hapi_trs, hapi_zyx).
{{hapi_result_success,3713831785},
 {hapi_transform,[0,0,0],
                 [0,0,0],
                 [1,1,1],
                 {hapi_zyx,2804301731},
                 {hapi_trs,2754525809}}}

> hapi:get_parm_id_from_name(3, "int_16").
{{hapi_result_success,3713831785},33}

> hapi:get_parm_info(3, 33).
{{hapi_result_success,3713831785},
 {hapi_parm_info,33,23,0,0,0,16,0,3266,3267,3269,3268,3269,
                 3268,false,false,true,true,0.0,10.0,0.0,10.0,false,false,
                 false,false,false,27...}}

> hapi:get_parm_int_values(3, 27, 16).
{{hapi_result_success,3713831785},
 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]}

> hapi:get_parm_id_from_name(3, "t").
 {{hapi_result_success,3713831785},51}

> hapi:get_parm_info(3, 51).
{{hapi_result_success,3713831785},
 {hapi_parm_info,51,3,4,0,0,3,0,3358,3357,3395,3380,3395,
                  3380,false,false,false,false,0.0,10.0,0.0,10.0,false,false,
                  false,false,false,28,...}}

> hapi:get_parm_float_values(3, 28, 3).
 {{hapi_result_success,3713831785},[0.0,0.0,0.0]}

> hapi:cleanup().
{hapi_result_success,3713831785}
```
## Supported HAPI calls ( 38 / 135, work in progress).

* hapi:initialize/5
* hapi:is_initialized/0
* hapi:cleanup/0
* hapi:get_env_int/1
* hapi:get_status/1
* hapi:get_status_string_buf_length/2
* hapi:get_status_string/1
* hapi:get_cooking_total_count/0
* hapi:get_cooking_current_count/0
* hapi:python_thread_interpreter_lock/1
* hapi:get_string_buf_length/1
* hapi:get_string/2
* hapi:get_time/0
* hapi:set_time/1
* hapi:get_timeline_options/0
* hapi:set_timeline_options/1
* hapi:is_asset_valid/2
* hapi:load_asset_library_from_file/2
* hapi:load_asset_library_from_memory/3
* hapi:get_available_asset_count/1
* hapi:get_available_assets/2
* hapi:instantiate_asset/2
* hapi:destroy_asset/1
* hapi:get_asset_info/1
* hapi:cook_asset/2
* hapi:interrupt/0
* hapi:get_asset_transform/3
* hapi:get_node_info/1
* hapi:get_parameters/3
* hapi:get_parm_info/2
* hapi:get_parm_id_from_name/2
* hapi:get_parm_info_from_name/2
* hapi:get_parm_int_value/3
* hapi:get_parm_float_value/3
* hapi:get_parm_int_values/3
* hapi:get_parm_float_values/3
* **Other HAPI calls are being added.**

## Additional helper calls.

* hapi:hash_enum_value/1
* hapi:check_enum_value_hash/2
