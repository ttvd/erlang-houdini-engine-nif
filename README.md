# Erlang NIF bindings for Houdini Engine.

## Goal.

Main goal of this project is to be able to run Houdini Engine instance (and be able to perform asset instantiation, cooking, etc) on an Erlang (as well as Elixir) node.

## Before building.  

Rakefile needs xxhash Ruby gem.

```
gem install xxhash
```

## Building.
Use rake (run rake without arguments to see available tasks).

```
$ rake erlang:generate
$ rake erlang:deps
$ rake erlang:compile
```

## Running.  

```
1> code:add_path("ebin").
true

2> hapi:is_initialized().
{hapi_result_not_initialized,2669115710}

3> hapi:initialize(nil, nil, nil, false, -1).
{hapi_result_success,3713831785}

4> hapi:is_initialized().
{hapi_result_success,3713831785}

5> hapi:cleanup().
{hapi_result_success,3713831785}

6> hapi:is_initialized().
{hapi_result_not_initialized,2669115710}

7> hapi:initialize("", "", {hapi_cook_options, false, 3, false, 8.0, false, false}, false, -1).
{hapi_result_success,3713831785}

8> hapi:is_initialized().
{hapi_result_success,3713831785}

9> % pass an atom as parameter.
9> hapi:get_env_int(hapi_envint_version_houdini_major).
{{hapi_result_success,3713831785},14}

10> % pass a hash of an atom as parameter.
10> hapi:get_env_int(729783216).
{{hapi_result_success,3713831785},14}

11> % pass a tuple of an atom and a hash as parameter.
11> hapi:get_env_int({hapi_envint_version_houdini_major, 729783216}).
{{hapi_result_success,3713831785},14}

12> hapi:get_env_int(hapi_envint_version_houdini_minor).
{{hapi_result_success,3713831785},5}

13> hapi:get_env_int(hapi_envint_version_houdini_build).
{{hapi_result_success,3713831785},80}

14> hapi:cleanup().
{hapi_result_success,3713831785}
```
## Supported HAPI calls (work in progress).

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
* **Other HAPI calls are being added.**
