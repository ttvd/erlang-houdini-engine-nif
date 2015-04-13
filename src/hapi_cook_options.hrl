%%% @author Mykola Konyk  <mykola@konyk.org>
%%%
%%% @copyright 2015
%%% HAPI_CookOptions structure.

% Record to hold fasta information.
-record(hapi_cook_options, {split_geos_by_group, max_vertices_per_primitive, refine_curve_to_linear, cureve_refine_lod,
    clear_errors_and_warnings, cook_template_geos}).
