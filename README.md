Erlang NIF bindings for Houdini Engine - Work in progress.
===============  

# Building.
Use rake (run rake without arguments to see available tasks).

# Running.  

```
1> code:add_path("ebin").
true

2> hapi:is_initialized().
hapi_result_not_initialized

3> hapi:initialize(nil, nil, nil, false, -1).
hapi_result_success

4> hapi:cleanup().
hapi_result_success
```
