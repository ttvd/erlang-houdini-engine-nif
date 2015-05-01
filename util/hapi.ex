defmodule HAPI do

    # Module used to generate HAPI C stub which we will parse.
    defmodule C do

        # Pre-process hapi.c which includes all hapi headers into something we can parse.
        def generate("clang", hapi_include_path) do
            {cmd_output, result_code} = System.cmd("clang", ["-cc1", "-ast-print", "-I#{hapi_include_path}", "./util/hapi.c"])
            if 0 == result_code do
                cmd_output
            else
                raise(RuntimeError, description: "Unable to expand macros in hapi.c")
            end
        end
        def generate("cpp.exe", hapi_include_path) do
            if File.exists?("./util/cpp.exe") do
                to_string(:os.cmd './util/cpp.exe -E -I"#{hapi_include_path}" ./util/hapi.c')
            else
                raise(RuntimeError, description: "xxhash utility was not compiled and is missing")
            end

        end
        def generate(_compiler, _hapi_include_path) do
            raise(RuntimeError, description: "Unknown compiler, please add options")
        end
    end

    # Lexical parsing.
    defmodule Lexical do

        # Create environment consisting of types, enums, structs and functions.
        def parse(data) do
            preprocess(data) |> tokenize()
        end

        # Remove preprocessor left overs from data stream.
        defp preprocess(data) do
            String.replace(data, ~r/int main\(\)\s\{\s*.*\s*\}/, "")
                |> String.replace(~r/#\s*\d+.*\n/, "")
                |> String.replace(~r/__attribute__\(\s*\(\s*visibility\(\s*\"default\"\s*\)\s*\)\s*\)\s+(\w+)/, "\\1")
                |> String.replace(~r/typedef\s+enum\s+\w+\s+\w+;/, "")
                |> String.replace(~r/typedef\s+struct\s+\w+\s+\w+;/, "")
                |> String.replace("__attribute__((visibility(0)))", "")
        end

        # Parse given string containing code.
        defp tokenize([]), do: []
        defp tokenize(code), do: parse_collect(code, "", [])

        # Parse and collect tokens.
        defp parse_collect("", _buf, tokens) do
            tokens
        end
        defp parse_collect(<<c>> <> rest, buf, tokens) do
            cond do
                is_whitespace(<<c>>) ->
                    parse_collect_submit(rest, buf, tokens)
                is_comma(<<c>>) ->
                    parse_collect_submit(rest, buf, tokens, :token_comma)
                is_semicolon(<<c>>) ->
                    parse_collect_submit(rest, buf, tokens, :token_semicolon)
                is_pointer(<<c>>) ->
                    parse_collect_submit(rest, buf, tokens, :token_pointer)
                is_bracket_left(<<c>>) ->
                    parse_collect_submit(rest, buf, tokens, :token_bracket_left)
                is_bracket_right(<<c>>) ->
                    parse_collect_submit(rest, buf, tokens, :token_bracket_right)
                is_bracket_curly_left(<<c>>) ->
                    parse_collect_submit(rest, buf, tokens, :token_bracket_curly_left)
                is_bracket_curly_right(<<c>>) ->
                    parse_collect_submit(rest, buf, tokens, :token_bracket_curly_right)
                is_bracket_square_left(<<c>>) ->
                    parse_collect_submit(rest, buf, tokens, :token_bracket_square_left)
                is_bracket_square_right(<<c>>) ->
                    parse_collect_submit(rest, buf, tokens, :token_bracket_square_right)
                is_assignment(<<c>>) ->
                    parse_collect_submit(rest, buf, tokens, :token_assignment)
                true ->
                    parse_collect(rest, buf <> <<c>>, tokens)
            end
        end

        # Helper method to collect and avoid empty token submission.
        defp parse_collect_submit(code, "", tokens), do: parse_collect(code, "", tokens)
        defp parse_collect_submit(code, buf, tokens), do: parse_collect(code, "", tokens ++ map_token(buf))
        defp parse_collect_submit(code, "", tokens, extra), do: parse_collect(code, "", tokens ++ [extra])
        defp parse_collect_submit(code, buf, tokens, extra), do: parse_collect(code, "", tokens ++ map_token(buf) ++ [extra])

        # Return true if current position is whitespace.
        defp is_whitespace(""), do: false
        defp is_whitespace(<<c>> <> _rest), do: String.match?(<<c>>, ~r/\s/)

        # Return true if current position is comma.
        defp is_comma(""), do: false
        defp is_comma("," <> _rest), do: true
        defp is_comma(_rest), do: false

        # Return true if current position is assignment.
        defp is_assignment(""), do: false
        defp is_assignment("=" <> _rest), do: true
        defp is_assignment(_rest), do: false

        # Return true if current position is semicolon.
        defp is_semicolon(""), do: false
        defp is_semicolon(";" <> _rest), do: true
        defp is_semicolon(_rest), do: false

        # Return true if current position is left bracket.
        defp is_bracket_left(""), do: false
        defp is_bracket_left("(" <> _rest), do: true
        defp is_bracket_left(_rest), do: false

        # Return true if current position is right bracket.
        defp is_bracket_right(""), do: false
        defp is_bracket_right(")" <> _rest), do: true
        defp is_bracket_right(_rest), do: false

        # Return true if current position is left curly bracket.
        defp is_bracket_curly_left(""), do: false
        defp is_bracket_curly_left("{" <> _rest), do: true
        defp is_bracket_curly_left(_rest), do: false

        # Return true if current position is right curly bracket.
        defp is_bracket_curly_right(""), do: false
        defp is_bracket_curly_right("}" <> _rest), do: true
        defp is_bracket_curly_right(_rest), do: false

        # Return true if current position is left square bracket.
        defp is_bracket_square_left(""), do: false
        defp is_bracket_square_left("[" <> _rest), do: true
        defp is_bracket_square_left(_rest), do: false

        # Return true if current position is right square bracket.
        defp is_bracket_square_right(""), do: false
        defp is_bracket_square_right("]" <> _rest), do: true
        defp is_bracket_square_right(_rest), do: false

        # Return true if current position is a pointer.
        defp is_pointer(""), do: false
        defp is_pointer("*"), do: true
        defp is_pointer(_rest), do: false

        # Map extracted string to a token.
        defp map_token(""), do: []
        defp map_token("typedef"), do: [:token_typedecl]
        defp map_token("enum"), do: [:token_enum]
        defp map_token("struct"), do: [:token_structure]
        defp map_token("const"), do: [:token_const]
        defp map_token("void"), do: [:token_void]
        defp map_token("float"), do: [:token_float]
        defp map_token("int"), do: [:token_int]
        defp map_token("char"), do: [:token_char]
        defp map_token("double"), do: [:token_double]
        defp map_token(token) do
            case Integer.parse(token) do
                {num, ""} ->
                    [num]
                _ ->
                    [token]
            end
        end

        # Function used to print token stream.
        def print_tokens(tokens) do
            Enum.map(tokens, fn(x) -> IO.puts("#{x}") end)
            tokens
        end
    end

    # Syntactic processing.
    defmodule Syntactic do

        # Given a list of tokens produce necessary tables.
        def process(tokens) do
            HashDict.new
                |> Dict.put(:types, HAPI.Syntactic.Types.process(tokens))
                |> Dict.put(:enums, HAPI.Syntactic.Enums.process(tokens))
                |> Dict.put(:structures, HAPI.Syntactic.Structs.process(tokens))
                |> Dict.put(:functions, HAPI.Syntactic.Functions.process(tokens))
        end

        # Type extraction into type table.
        defmodule Types do

            # Given a list of tokens, produce a mapping table (parsed type -> system type).
            def process(tokens) do
                add_entry = &(Dict.put(&1, &2, HAPI.Util.get_builtin_type(&2)))
                HashDict.new
                    |> add_entry.("void")
                    |> add_entry.("int")
                    |> add_entry.("float")
                    |> add_entry.("double")
                    |> add_entry.("bool")
                    |> add_entry.("char")
                    |> process_collect(tokens)
            end

            # Process tokens and collect types.
            defp process_collect(dict, []), do: dict
            defp process_collect(dict, [:token_typedecl, _type_origin, "HAPI_Bool" | rest]) do
                Dict.put(dict, "HAPI_Bool", :token_bool)
                    |> process_collect(rest)
            end
            defp process_collect(dict, [:token_typedecl, type_origin, type_new | rest]) do
                Dict.put(dict, type_new, type_origin)
                    |> process_collect(rest)
            end
            defp process_collect(dict, [:token_enum, enum_name | rest]) do
                Dict.put(dict, enum_name, :token_enum)
                    |> process_collect(rest)
            end
            defp process_collect(dict, [:token_structure, struct_name | rest]) do
                Dict.put(dict, struct_name, :token_structure)
                    |> process_collect(rest)
            end
            defp process_collect(dict, [_token | rest]), do: process_collect(dict, rest)
        end

        # Enum extraction into enum table.
        defmodule Enums do

            # Given a list of tokens, produce a mapping table for enums.
            def process(tokens) do
                HashDict.new
                    |> process_collect(tokens)
            end

            # Process tokens and collect enums.
            defp process_collect(dict, []), do: dict
            defp process_collect(dict, [:token_enum, enum_name, :token_bracket_curly_left | rest]) do
                {enum_values, remaining} = process_extract([], rest, 0)
                Dict.put(dict, enum_name, enum_values)
                    |> process_collect(remaining)
            end
            defp process_collect(_dict, [:token_enum | _rest]), do: raise(SyntaxError, description: "Invalid enum detected")
            defp process_collect(dict, [_token | rest]), do: process_collect(dict, rest)

            # Helper function to extract enum values from token stream.
            defp process_extract(_values, [], _idx), do: raise(SyntaxError, description: "Unexpected end of enum")
            defp process_extract(values, [:token_comma | rest], idx), do: process_extract(values, rest, idx)
            defp process_extract(values, [:token_bracket_curly_right, :token_semicolon | rest], _idx), do: {values, rest}
            defp process_extract(values, [enum_entry, :token_comma | rest], idx) do
                values ++ [{enum_entry, idx}]
                    |> process_extract(rest, idx + 1)
            end
            defp process_extract(values, [enum_entry, :token_bracket_curly_right, :token_semicolon | rest], idx) do
                {values ++ [{enum_entry, idx}], rest}
            end
            defp process_extract(values, [enum_entry, :token_assignment, enum_value | rest], _idx) when is_integer(enum_value) do
                values ++ [{enum_entry, enum_value}]
                    |> process_extract(rest, enum_value + 1)
            end
            defp process_extract(values, [enum_entry, :token_assignment, enum_value | rest], _idx) do
                orig_value = process_lookup(values, values, enum_value)
                values ++ [{enum_entry, orig_value, enum_value}]
                    |> process_extract(rest, orig_value + 1)
            end

            # Helper function used to look up enum value within enum table.
            defp process_lookup([], _values, _enum_value), do: raise(SyntaxError, description: "Enum value was not found")
            defp process_lookup([field | rest], values, enum_value) do
                if elem(field, 0) == enum_value do
                    field_value = elem(field, 1)
                    if is_integer(field_value) do
                        field_value
                    else
                        process_lookup(values, values, field_value)
                    end
                else
                    process_lookup(rest, values, enum_value)
                end
            end
        end

        # Struct extraction into struct table.
        defmodule Structs do

            # Given a list of tokens, produce a mapping table for structs.
            def process(tokens) do
                HashDict.new
                    |> process_collect(tokens)
            end

            # Process tokens and collect structures.
            defp process_collect(dict, []), do: dict
            defp process_collect(dict, [:token_structure, struct_name, :token_bracket_curly_left | rest]) do
                [struct_body, remaining] = process_extract([], rest)
                Dict.put(dict, struct_name, struct_body)
                    |> process_collect(remaining)
            end
            defp process_collect(_dict, [:token_structure | _rest]) do
                raise(SyntaxError, description: "Invalid struct detected")
            end
            defp process_collect(dict, [_token | rest]), do: process_collect(dict, rest)

            # Helper function to extract struct fields from token stream.
            defp process_extract(_list, []), do: raise(SyntaxError, description: "Unexpected end of struct")
            defp process_extract(list, [:token_bracket_curly_right, :token_semicolon | rest]), do: [list, rest]
            defp process_extract(list, [field_type, field_name, :token_bracket_square_left, field_size,
                :token_bracket_square_right, :token_semicolon | rest]) do
                    list ++ [{field_name, field_type, field_size}]
                        |> process_extract(rest)
            end
            defp process_extract(list, [field_type, field_name, :token_semicolon | rest]) do
                list ++ [{field_name, field_type}]
                    |> process_extract(rest)
            end
        end

        # Function extraction into function table.
        defmodule Functions do

            # Given a list of tokens, produce a mapping table for functions.
            def process(tokens) do
                HashDict.new
                    |> process_collect(tokens)
            end

            # Process tokens and collect functions.
            defp process_collect(dict, []), do: dict
            defp process_collect(dict, [function_type, function_name, :token_bracket_left | rest]) do
                [params, remaining] = process_extract([], rest)
                Dict.put(dict, function_name, {function_type, params})
                    |> process_collect(remaining)
            end
            defp process_collect(dict, [_token | rest]), do: process_collect(dict, rest)

            # Helper function to extract function parameters.
            defp process_extract(_list, []), do: raise(SyntaxError, description: "Unexpected end of function")
            defp process_extract(list, [:token_comma | rest]), do: process_extract(list, rest)
            defp process_extract(list, [:token_bracket_right, :token_semicolon | rest]), do: [list, rest]
            defp process_extract(list, [:token_const, param_type, :token_pointer, :token_pointer, param_name | rest] = tokens) do
                list ++ [{param_type, param_name, process_flags(HashDict.new, Enum.take(tokens, 4))}]
                        |> process_extract(rest)
            end
            defp process_extract(list, [:token_const, :token_char, :token_pointer, param_name | rest] = tokens) do
                list ++ [{:token_char, param_name,
                            process_flags(HashDict.new |> Dict.put(:param_string, true), Enum.take(tokens, 4))}]
                    |> process_extract(rest)
            end
            defp process_extract(list, [:token_const, param_type, :token_pointer, param_name | rest] = tokens) do
                list ++ [{param_type, param_name, process_flags(HashDict.new, Enum.take(tokens, 4))}]
                    |> process_extract(rest)
            end
            defp process_extract(list, [:token_const, param_type, param_name | rest] = tokens) do
                list ++ [{param_type, param_name, HashDict.new |> process_flags(Enum.take(tokens, 3))}]
                    |> process_extract(rest)
            end
            defp process_extract(list, [:token_char, :token_pointer, param_name | rest] = tokens) do
                list ++ [{:token_char, param_name,
                            process_flags(HashDict.new |> Dict.put(:param_string, true), Enum.take(tokens, 3))}]
                    |> process_extract(rest)
            end
            defp process_extract(list, [param_type, :token_pointer, param_name | rest] = tokens) do
                list ++ [{param_type, param_name, process_flags(HashDict.new, Enum.take(tokens, 3))}]
                    |> process_extract(rest)
            end
            defp process_extract(list, [param_type, param_name | rest] = tokens) do
                list ++ [{param_type, param_name, process_flags(HashDict.new, Enum.take(tokens, 2))}]
                    |> process_extract(rest)
            end

            # Helper function used add flags.
            defp process_flags(dict, []), do: dict
            defp process_flags(dict, [:token_const | rest]) do
                Dict.put(dict, :param_const, true)
                    |> process_flags(rest)
            end
            defp process_flags(dict, [:token_pointer | rest]) do
                if Dict.get(dict, :param_pointer, false) do
                    Dict.put(dict, :param_pointer_pointer, true)
                        |> process_flags(rest)
                else
                    Dict.put(dict, :param_pointer, true)
                        |> process_flags(rest)
                end
            end
            defp process_flags(dict, [param | rest]) when is_binary(param) do
                if String.match?(param, ~r/\w+es$/) or String.match?(param, ~r/\w+s$/) do
                    Dict.put(dict, :param_array, true)
                        |> process_flags(rest)
                else
                    process_flags(dict, rest)
                end
            end
            defp process_flags(dict, [_param | rest]), do: process_flags(dict, rest)
        end

        # Print types.
        def print_types(env) do
            types = Dict.get(env, :types, :nil)
            if not is_nil(types) do
                Enum.map(types, fn {k, v} -> IO.puts("#{k} -> #{v}") end)
            end
            env
        end

        # Print enums.
        def print_enums(env) do
            enums = Dict.get(env, :enums, :nil)
            if not is_nil(enums) do
                Enum.map(enums, fn {k, v} -> print_enum(k, v) end)
            end
            env
        end

        # Helper function to print each individual enum.
        defp print_enum(enum_name, enum_body) do
            IO.puts("#{enum_name}")
            Enum.map(enum_body, fn(x) -> print_enum_field(x) end)
            IO.puts("")
        end

        # Helper function to print each enum field.
        defp print_enum_field({field_name, field_value, field_original}) do
            IO.puts("    #{field_name} -> #{field_value} -> #{field_original}")
        end
        defp print_enum_field({field_name, field_value}), do: IO.puts("    #{field_name} -> #{field_value}")

        # Helper function to print each individual struct.
        def print_structs(env) do
            structs = Dict.get(env, :structs, :nil)
            if not is_nil(structs) do
                Enum.map(structs, fn {k, v} -> print_struct(k, v) end)
            end
            env
        end

        # Helper function to print each individual struct.
        defp print_struct(struct_name, struct_body) do
            IO.puts("#{struct_name}")
            Enum.map(struct_body, fn(x) -> print_struct_field(x) end)
            IO.puts("")
        end

        # Helper function to print each individual struct field.
        defp print_struct_field({field_name, field_type, field_size}) do
            IO.puts("    #{field_type} #{field_name}[#{field_size}]")
        end
        defp print_struct_field({field_name, field_type}) do
            IO.puts("    #{field_type} #{field_name}")
        end

        # Print functions.
        def print_functions(env) do
            funcs = Dict.get(env, :functions, :nil)
            if not is_nil(funcs) do
                Enum.map(funcs, fn {k, v} -> print_function(k, v) end)
            end
            env
        end

        # Helper function to print each individual function.
        defp print_function(function_name, {function_type, function_params}) do
            IO.puts("#{function_name} -> #{function_type}")
            Enum.map(function_params, fn(param) -> print_function_param(param) end)
            IO.puts("")
        end

        # Helper function to print each individual function parameter.
        defp print_function_param({param_type, param_name, param_opts}) do
            IO.puts("    #{param_type} #{param_name}")
            Enum.map(param_opts, fn {k, v} -> IO.puts("        #{k} -> #{v}") end)
        end
    end

    # Utility module.
    defmodule Util do

        # Helper function to retrieve number of fields in a given structure.
        def get_struct_field_count(_struct_name, struct_body) do
            Integer.to_string(length(struct_body) + 1)
        end

        # Helper function to generate hash for a given string.
        def hash(string) when is_binary(string) do
            if File.exists?("./util/xxhash") do
                to_string(:os.cmd './util/xxhash #{string}')
            else
                raise(RuntimeError, description: "xxhash utility was not compiled and is missing")
            end
        end
        def hash(_string) do
            raise(RuntimeError, description: "Can't create hash of non-binary parameter")
        end

        # Helper function to map type names to types.
        def get_builtin_type("void"), do: :token_void
        def get_builtin_type("int"), do: :token_int
        def get_builtin_type("float"), do: :token_float
        def get_builtin_type("double"), do: :token_double
        def get_builtin_type("bool"), do: :token_bool
        def get_builtin_type("char"), do: :token_char
        def get_builtin_type(_type), do: :nil

        # Helper function to reverse map type names to types.
        def get_reverse_builtin_type(_env, :token_void), do: "void"
        def get_reverse_builtin_type(_env, :token_int), do: "int"
        def get_reverse_builtin_type(_env, :token_float), do: "float"
        def get_reverse_builtin_type(_env, :token_double), do: "double"
        def get_reverse_builtin_type(_env, :token_bool), do: "bool"
        def get_reverse_builtin_type(_env, :token_char), do: "char"
        def get_reverse_builtin_type(_env, _type), do: :nil

        # Helper function to return original type.
        def get_original_type(env, type) do
            types = Dict.get(env, :types, :nil)
            if not is_nil(types) do
                Dict.get(types, type, :nil)
            else
                :nil
            end
        end

        # Helper function to check if type name is not typedef'ed.
        def is_type_builtin(_env, type), do: not is_nil(get_builtin_type(type))

        # Helper function to check if type is enum.
        def is_type_enum(_env, :token_enum), do: true
        def is_type_enum(_env, :nil), do: false
        def is_type_enum(env, type) do
            types = Dict.get(env, :types, :nil)
            if not is_nil(types) do
                is_type_enum(env, Dict.get(types, type, :nil))
            else
                false
            end
        end

        # Helper function to check if type is a struct.
        def is_type_structure(_env, :token_structure), do: true
        def is_type_structure(_env, :nil), do: false
        def is_type_structure(env, type) do
            types = Dict.get(env, :types, :nil)
            if not is_nil(types) do
                is_type_structure(env, Dict.get(types, type, :nil))
            else
                false
            end
        end

        # Helper function to check if type is a primitive type.
        def is_type_primitive(_env, :token_int), do: true
        def is_type_primitive(_env, :token_bool), do: true
        def is_type_primitive(_env, :token_float), do: true
        def is_type_primitive(_env, :token_char), do: true
        def is_type_primitive(_env, :token_double), do: true
        def is_type_primitive(_env, :nil), do: false
        def is_type_primitive(env, type) do
            types = Dict.get(env, :types, :nil)
            if not is_nil(types) do
                is_type_primitive(env, Dict.get(types, type, :nil))
            else
                false
            end
        end

        # Helper function to look up a type.
        def type_lookup(env, type) do
            types = Dict.get(env, :types, :nil)
            if not is_nil(types) do
                Dict.get(types, type, type)
            else
                type
            end
        end

        # Helper function to translate character to lower case.
        def to_lower_case(c) when c in ?A..?Z, do: c + 32
        def to_lower_case(c), do: c

        # Helper function to create underscore version of a given string.
        def underscore(""), do: ""
        def underscore(string) when is_binary(string), do: underscore_helper(string)
        def underscore(_string) do
            raise(RuntimeError, description: "Can't create underscore version of non-binary parameter")
        end
        defp underscore_helper(<<c, rest ::binary>>), do: <<to_lower_case(c)>> <> underscore_helper(rest, c)
        defp underscore_helper(<<c, rest ::binary>>, p) when p == ?_ do
            <<to_lower_case(c)>> <> underscore_helper(rest, c)
        end
        defp underscore_helper(<<c, rest ::binary>>, p) when c in ?A..?Z and not p in ?A..?Z do
            <<?_, to_lower_case(c)>> <> underscore_helper(rest, c)
        end
        defp underscore_helper(<<c, rest ::binary>>, _p), do: <<to_lower_case(c)>> <> underscore_helper(rest, c)
        defp underscore_helper("", _p), do: ""
    end

    # Module responsible for generating type related stubs.
    defmodule Types do

        # Create type related stubs.
        def create(env) do
            if not (Dict.get(env, :types, :nil) |> is_nil()) do
                create_stub_h(env)
                create_stub_c(env)
            end
            env
        end

        # Create header stub for types.
        defp create_stub_h(env) do
            types = Dict.get(env, :types, :nil)

            {:ok, template_types_h} = File.read("./util/hapi_types_nif.h.template")
            {:ok, template_types_h_block} = File.read("./util/hapi_types_nif.h.block.template")

            entries = Enum.map(types, fn {k, _v} -> k end)
                |> Enum.filter(fn(x) -> not HAPI.Util.is_type_builtin(env, x) end)
                |> Enum.map(fn(x) -> create_stub_h_entry(env, x, template_types_h_block) end)
                |> Enum.filter(fn(x) -> not is_nil(x) end)
                |> Enum.join("\n")

            entries = String.replace(template_types_h, "%{HAPI_TYPE_FUNCTIONS}%", entries)

            File.write("./c_src/hapi_types_nif.h", entries)
            IO.puts("Generating c_src/hapi_types_nif.h")
        end

        # Helper function to create types header file entry.
        def create_stub_h_entry(env, type_name, template) do
            if HAPI.Util.is_type_primitive(env, type_name) do
                String.replace(template, "%{HAPI_TYPE_DOWNCASE}%", HAPI.Util.underscore(type_name))
                    |> String.replace("%{HAPI_TYPE}%", type_name)
            else
                :nil
            end
        end

        # Create source file stub for types.
        defp create_stub_c(env) do
            types = Dict.get(env, :types, :nil)

            {:ok, template_types_c} = File.read("./util/hapi_types_nif.c.template")
            {:ok, template_types_c_block} = File.read("./util/hapi_types_nif.c.block.template")

            entries = Enum.map(types, fn {k, v} -> create_stub_c_entry(env, k, v, template_types_c_block) end)
                |> Enum.filter(fn(x) -> not is_nil(x) end)
                |> Enum.join("\n")

            entries = String.replace(template_types_c, "%{HAPI_TYPE_FUNCTIONS}%", entries)

            File.write("./c_src/hapi_types_nif.c", entries)
            IO.puts("Generating c_src/hapi_types_nif.c")
        end

        # Helper function to create type conversion functions.
        defp create_stub_c_entry(env, type_name, token_type, template) do
            if HAPI.Util.is_type_primitive(env, type_name) do
                String.replace(template, "%{HAPI_TYPE_DOWNCASE}%", HAPI.Util.underscore(type_name))
                    |> String.replace("%{HAPI_TYPE}%", type_name)
                    |> create_stub_c_entry_code(env, type_name, token_type)
            else
                :nil
            end
        end

        # Helper function to generate code for converting type.
        defp create_stub_c_entry_code(template, env, type_name, _token_type) do
            cond do
                HAPI.Util.is_type_builtin(env, type_name) ->
                    :nil
                type_name == "HAPI_Bool" ->
                    String.replace(template, "%{HAPI_TYPE_CONVERT_MAKE}%", "return hapi_make_bool(env, (bool) hapi_type);")
                        |> String.replace("%{HAPI_TYPE_CONVERT_GET}%", "return hapi_get_bool(env, term, (bool*) hapi_type);")
                true ->
                    old_type = HAPI.Util.get_reverse_builtin_type(env, HAPI.Util.get_original_type(env, type_name))
                    String.replace(template, "%{HAPI_TYPE_CONVERT_MAKE}%",
                        "return hapi_make_#{old_type}(env, (#{old_type}) hapi_type);")
                        |> String.replace("%{HAPI_TYPE_CONVERT_GET}%",
                            "return hapi_get_#{old_type}(env, term, (#{old_type}*) hapi_type);")
            end
        end
    end

    # Module responsible for generating enum related stubs.
    defmodule Enums do

        # Create enum related stubs.
        def create(env) do
            if not (Dict.get(env, :enums, :nil) |> is_nil()) do
                create_stub_h(env)
                create_stub_c(env)
            end
            env
        end

        # Create header stub for enums.
        defp create_stub_h(env) do
            enums = Dict.get(env, :enums, :nil)
            if not is_nil(enums) do
                {:ok, template_enums_h} = File.read("./util/hapi_enums_nif.h.template")
                {:ok, template_enums_block} = File.read("./util/hapi_enums_nif.h.block.template")

                signature_blocks = Enum.map_join(enums, "\n", fn {k, _v} ->
                    String.replace(template_enums_block, "%{HAPI_ENUM}%", k)
                        |> String.replace("%{HAPI_ENUM_DOWNCASE}%", HAPI.Util.underscore(k)) end)
                signatures = String.replace(template_enums_h, "%{HAPI_ENUM_FUNCTIONS}%", signature_blocks)

                File.write("./c_src/hapi_enums_nif.h", signatures)
                IO.puts("Generating c_src/hapi_enums_nif.h")
            end
        end

        # Create source stub for enums.
        defp create_stub_c(env) do
            enums = Dict.get(env, :enums, :nil)
            if not is_nil(enums) do
                {:ok, template_enums_c} = File.read("./util/hapi_enums_nif.c.template")
                {:ok, template_enums_c_block} = File.read("./util/hapi_enums_nif.c.block.template")
                {:ok, template_enums_c_erl_to_c_block} = File.read("./util/hapi_enums_nif.c.erl_to_c.block.template")
                {:ok, template_enums_c_c_to_erl_block} = File.read("./util/hapi_enums_nif.c.c_to_erl.block.template")

                enum_code = String.replace(template_enums_c, "%{HAPI_ENUM_FUNCTIONS}%",
                    Enum.map_join(enums, "\n\n", fn {k, v} -> create_stub_c_entry(k, v, template_enums_c_block,
                        template_enums_c_erl_to_c_block, template_enums_c_c_to_erl_block) end))

                File.write("./c_src/hapi_enums_nif.c", enum_code)
                IO.puts("Generating c_src/hapi_enums_nif.c")
            end
        end

        # Helper function to create enum conversion function pair.
        def create_stub_c_entry(enum_name, enum_body, template_block, template_erl_to_c, template_c_to_erl) do
            c_to_erl_blocks = Enum.map(enum_body,
                fn(f) -> create_stub_c_entry_c_to_erl(template_c_to_erl, f) end)
                    |> Enum.filter(fn(f) -> not is_nil(f) end) |> Enum.join("\n")
            erl_to_c_blocks = Enum.map_join(enum_body, "\n",
                fn(f) -> create_stub_c_entry_erl_to_c(template_erl_to_c, elem(f, 0)) end)

            String.replace(template_block, "%{HAPI_ENUM}%", enum_name)
                |> String.replace("%{HAPI_ENUM_DOWNCASE}%", HAPI.Util.underscore(enum_name))
                |> String.replace("%{HAPI_ENUM_C_TO_ERL_BODY}%", c_to_erl_blocks)
                |> String.replace("%{HAPI_ENUM_ERL_TO_C_BODY}%", erl_to_c_blocks)
        end

        # Function to generate c_to_erl block for c <-> erl enum c stub.
        defp create_stub_c_entry_c_to_erl(_template_c_to_erl, {_field_name, _field_value, _field_original}), do: :nil
        defp create_stub_c_entry_c_to_erl(template_c_to_erl, {field_name, _field_value}) do
            [String.replace(template_c_to_erl, "%{HAPI_ENUM_VALUE}%", field_name)
                |> String.replace("%{HAPI_ENUM_VALUE_DOWNCASE}%", HAPI.Util.underscore(field_name))]
        end

        # Function to generate erl_to_c block for c <-> erl enum c stub.
        defp create_stub_c_entry_erl_to_c(template_erl_to_c, field_name) do
            field_name_underscore = HAPI.Util.underscore(field_name)
            [String.replace(template_erl_to_c, "%{HAPI_ENUM_VALUE}%", field_name)
                |> String.replace("%{HAPI_ENUM_VALUE_DOWNCASE}%", field_name_underscore)
                |> String.replace("%{HAPI_ENUM_HASH}%", HAPI.Util.hash(field_name_underscore))]
        end
    end

    # Module responsible for generating structure related stubs.
    defmodule Structures do

        # Create structure related stubs.
        def create(env) do
            if not (Dict.get(env, :structures, :nil) |> is_nil()) do
                create_stub_hrl(env)
                create_stub_h(env)
                create_stub_c(env)
            end
            env
        end

        # Function used to generate erl hrl stub for structures.
        defp create_stub_hrl(env) do
            structures = Dict.get(env, :structures, :nil)
            if not is_nil(structures) do
                {:ok, template_structs_hrl} = File.read("./util/hapi_records.hrl.template")
                {:ok, template_structs_hrl_block} = File.read("./util/hapi_records.hrl.block.template")

                records = String.replace(template_structs_hrl, "%{HAPI_RECORDS}%",
                    Enum.map_join(structures, "\n\n", fn{k, v} -> create_stub_hrl_entry(k, v, template_structs_hrl_block) end))

                File.write("./src/hapi_records.hrl", records)
                IO.puts("Generating src/hapi_records.hrl")
            end
        end

        # Helper function to produce hrl record entry.
        defp create_stub_hrl_entry(structure_name, structure_body, template_block) do
            String.replace(template_block, "%{HAPI_STRUCT}%", structure_name)
                |> String.replace("%{HAPI_RECORD_NAME}%", HAPI.Util.underscore(structure_name))
                |> String.replace("%{HAPI_RECORD_ENTRIES}%",
                    Enum.map_join(structure_body, ",\n    ", fn(f) -> "#{HAPI.Util.underscore(elem(f, 0))}" end))
        end

        # Function used to generate header stub for structures.
        defp create_stub_h(env) do
            structures = Dict.get(env, :structures, :nil)
            if not is_nil(structures) do
                {:ok, template_structures_h} = File.read("./util/hapi_structures_nif.h.template")
                {:ok, template_structures_h_block} = File.read("./util/hapi_structures_nif.h.block.template")

                signatures = String.replace(template_structures_h, "%{HAPI_STRUCT_FUNCTIONS}%",
                    Enum.map_join(structures, "\n", fn{k, _v} -> create_stub_h_entry(k, template_structures_h_block) end))

                File.write("./c_src/hapi_structures_nif.h", signatures)
                IO.puts("Generating c_src/hapi_structures_nif.h")
            end
        end

        # Helper function to produce header stub entry.
        defp create_stub_h_entry(structure_name, template_block) do
            structure_name_underscore = HAPI.Util.underscore(structure_name)
            String.replace(template_block, "%{HAPI_STRUCT_DOWNCASE}%", structure_name_underscore)
                |> String.replace("%{HAPI_STRUCT}%", structure_name)
        end

        # Function used to generate source file stub for structures.
        defp create_stub_c(env) do
            structures = Dict.get(env, :structures, :nil)
            if not is_nil(structures) do
                {:ok, template_structures_c} = File.read("./util/hapi_structures_nif.c.template")
                {:ok, template_structures_c_block} = File.read("./util/hapi_structures_nif.c.block.template")

                struct_entries = String.replace(template_structures_c, "%{HAPI_STRUCT_FUNCTIONS}%",
                    Enum.map_join(structures, "\n", fn{k, v} -> create_stub_c_entry(env, k, v, template_structures_c_block) end))

                File.write("./c_src/hapi_structures_nif.c", struct_entries)
                IO.puts("Generating c_src/hapi_structures_nif.c")
            end
        end

        # Helper function to create structure c stub entry.
        defp create_stub_c_entry(env, struct_name, struct_body, template) do
            String.replace(template, "%{HAPI_STRUCT}%", struct_name)
                |> String.replace("%{HAPI_STRUCT_DOWNCASE}%", HAPI.Util.underscore(struct_name))
                |> String.replace("%{HAPI_STRUCT_SIZE}%", HAPI.Util.get_struct_field_count(struct_name, struct_body))
                |> String.replace("%{HAPI_STRUCT_TO_ERL_MAP}",
                    Enum.map_join(struct_body, ",\n        ",
                        fn(f) -> create_stub_c_entry_c_to_erl(env, f) end))
                |> String.replace("%{HAPI_STRUCT_TO_C_VARS}%",
                    Enum.map_join(struct_body, "\n    ",
                        fn(f) -> create_stub_c_entry_erl_to_c_var(env, f) end))
                |> String.replace("%{HAPI_STRUCT_TO_C_ASSIGN}%",
                    Enum.map_join(struct_body, "\n    ",
                        fn(f) -> create_stub_c_entry_erl_to_c_assign(env, f) end))
                |> String.replace("%{HAPI_STRUCT_TO_C_MAP}%",
                    Enum.map_join(Enum.with_index(struct_body), "||\n        ",
                        fn(f) -> create_stub_c_entry_erl_to_c_extract(env, f) end))
        end

        # Helper function to create calls necessary to produce erl record.
        defp create_stub_c_entry_c_to_erl(env, {field_name, field_type}) do
            builtin_type = HAPI.Util.get_reverse_builtin_type(env, field_type)
            cond do
                not is_nil(builtin_type) ->
                    "hapi_make_#{HAPI.Util.underscore(builtin_type)}(env, hapi_struct->#{field_name})"
                HAPI.Util.is_type_structure(env, field_type) ->
                    "hapi_make_#{HAPI.Util.underscore(field_type)}(env, &hapi_struct->#{field_name})"
                true ->
                    "hapi_make_#{HAPI.Util.underscore(field_type)}(env, hapi_struct->#{field_name})"
            end
        end
        defp create_stub_c_entry_c_to_erl(env, {field_name, field_type, field_size}) do
            builtin_type = HAPI.Util.get_reverse_builtin_type(env, field_type)
            field_target = "hapi_struct->#{field_name}[0]"
            cond do
                not is_nil(builtin_type) ->
                    "hapi_make_#{HAPI.Util.underscore(builtin_type)}_list(env, &#{field_target}, #{field_size})"
                HAPI.Util.is_type_structure(env, field_type) ->
                    "hapi_make_#{HAPI.Util.underscore(field_type)}_list(env, &#{field_target}, #{field_size})"
                true ->
                    "hapi_make_#{HAPI.Util.underscore(field_type)}_list(env, &#{field_target}, #{field_size})"
            end
        end

        # Helper function to create variable declarations in erl to c conversion functions.
        defp create_stub_c_entry_erl_to_c_var(env, {field_name, field_type}) do
            builtin_type = HAPI.Util.get_reverse_builtin_type(env, field_type)
            field_name_underscore = "field_#{HAPI.Util.underscore(field_name)}"
            cond do
                not is_nil(builtin_type) ->
                    "#{builtin_type} #{field_name_underscore};"
                true ->
                    "#{field_type} #{field_name_underscore};"
            end
        end
        defp create_stub_c_entry_erl_to_c_var(env, {field_name, field_type, field_size}) do
            builtin_type = HAPI.Util.get_reverse_builtin_type(env, field_type)
            field_name_underscore = "field_#{HAPI.Util.underscore(field_name)}[#{field_size}]"
            cond do
                not is_nil(builtin_type) ->
                    "#{builtin_type} #{field_name_underscore};"
                true ->
                    "#{field_type} #{field_name_underscore};"
            end
        end

        # Helper function to create variable assignment.
        defp create_stub_c_entry_erl_to_c_assign(env, {field_name, field_type}) do
            field_name_underscore = "field_#{HAPI.Util.underscore(field_name)}"
            if HAPI.Util.is_type_structure(env, field_type) do
                "memcpy(&hapi_struct->#{field_name}, &#{field_name_underscore}, sizeof(#{field_type}));"
            else
                "hapi_struct->#{field_name} = #{field_name_underscore};"
            end
        end
        defp create_stub_c_entry_erl_to_c_assign(env, {field_name, field_type, field_size}) do
            builtin_type = HAPI.Util.get_reverse_builtin_type(env, field_type)
            field_from = "hapi_struct->#{field_name}[0]"
            field_name_underscore = "field_#{HAPI.Util.underscore(field_name)}[0]"
            cond do
                not is_nil(builtin_type) ->
                    "memcpy(&#{field_from}, &#{field_name_underscore}, #{field_size} * sizeof(#{builtin_type}));"
                true ->
                    "memcpy(&#{field_from}, &#{field_name_underscore}, #{field_size} * sizeof(#{field_type}));"
            end
        end

        # Helper function to create extraction of data into local variables.
        defp create_stub_c_entry_erl_to_c_extract(env, {{field_name, field_type}, idx}) do
            builtin_type = HAPI.Util.get_reverse_builtin_type(env, field_type)
            field_name_underscore = "field_#{HAPI.Util.underscore(field_name)}"
            term = "tuple_record[#{idx + 1}]"
            cond do
                not is_nil(builtin_type) ->
                    "!hapi_get_#{HAPI.Util.underscore(builtin_type)}(env, #{term}, &#{field_name_underscore})"
                HAPI.Util.is_type_structure(env, field_type) ->
                    "!hapi_get_#{HAPI.Util.underscore(field_type)}(env, #{term}, &#{field_name_underscore})"
                true ->
                    "!hapi_get_#{HAPI.Util.underscore(field_type)}(env, #{term}, &#{field_name_underscore})"
            end
        end
        defp create_stub_c_entry_erl_to_c_extract(env, {{field_name, field_type, field_size}, idx}) do
            builtin_type = HAPI.Util.get_reverse_builtin_type(env, field_type)
            field_target = "hapi_struct->#{field_name}[0]"
            term = "tuple_record[#{idx + 1}]"
            cond do
                not is_nil(builtin_type) ->
                    "hapi_get_#{HAPI.Util.underscore(builtin_type)}_list(env, #{term}, &#{field_target}, #{field_size})"
                HAPI.Util.is_type_structure(env, field_type) ->
                    "hapi_get_#{HAPI.Util.underscore(field_type)}_list(env, #{term}, &#{field_target}, #{field_size})"
                true ->
                    "hapi_get_#{HAPI.Util.underscore(field_type)}_list(env, #{term}, &#{field_target}, #{field_size})"
            end
        end
    end

    # Module responsible for generating function related stubs.
    defmodule Functions do

        # Create structure related stubs.
        def create(env) do
            if not (Dict.get(env, :structures, :nil) |> is_nil()) do
                create_stub_exports(env)
                create_stub_h(env)
                create_stub_c(env)
            end
            env
        end

        # Helper function, given a function structure, return parameters (ignore parameters returned by pointer).
        defp get_parameters({_function_type, function_params}) do
            Enum.filter(function_params, &(not is_return_parameter(&1)))
        end

        # Helper function to check if parameter is a return type parameter.
        defp is_return_parameter({_param_type, _param_name, dict}) do
            Dict.get(dict, :param_pointer, false) and not Dict.get(dict, :param_const, false)
        end

        # Function used to generate export table.
        defp create_stub_exports(env) do
            functions = Dict.get(env, :functions, :nil)
            if not is_nil(functions) do
                {:ok, template_exports_c} = File.read("./util/hapi_exports_nif.c.template")

                exports = String.replace(template_exports_c, "%{HAPI_NIF_FUNCTIONS}%",
                    Enum.map_join(functions, ",\n    ",
                        fn{k, v} -> create_stub_exports_entry(k, length(get_parameters(v))) end))

                File.write("./c_src/hapi_exports_nif.c", exports)
                IO.puts("Generating c_src/hapi_exports_nif.c")
            end
        end

        # Helper function used to generate each export entry.
        defp create_stub_exports_entry("HAPI_" <> function_name, arity) do
            function_name_underscore = HAPI.Util.underscore(function_name)
            "{\"#{function_name_underscore}\", #{Integer.to_string(arity)}, hapi_#{function_name_underscore}}"
        end

        # Function used to generate h stub.
        defp create_stub_h(env) do
            functions = Dict.get(env, :functions, :nil)
            if not is_nil(functions) do
                {:ok, template_functions_h} = File.read("./util/hapi_functions_nif.h.template")
                {:ok, template_functions_h_block} = File.read("./util/hapi_functions_nif.h.block.template")

                signatures = String.replace(template_functions_h, "%{HAPI_FUNCTIONS}%",
                    Enum.map_join(functions, "", fn{k, _v} -> create_stub_h_entry(k, template_functions_h_block) end))

                File.write("./c_src/hapi_functions_nif.h", signatures)
                IO.puts("Generating c_src/hapi_functions_nif.h")
            end
        end

        # Helper function to create h stub entries.
        defp create_stub_h_entry(function_name, template_block) do
            String.replace(template_block, "%{HAPI_FUNCTION}%", HAPI.Util.underscore(function_name))
        end

        # Function used to generate c stub.
        defp create_stub_c(env) do
            functions = Dict.get(env, :functions, :nil)
            if not is_nil(functions) do
                {:ok, template_functions_c} = File.read("./util/hapi_functions_nif.c.template")
                {:ok, template_functions_c_block} = File.read("./util/hapi_functions_nif.c.block.template")

                entries = String.replace(template_functions_c, "%{HAPI_FUNCTIONS}%",
                    Enum.map_join(functions, "\n\n", fn{k, v} -> create_stub_c_entry(env, k, v, template_functions_c_block) end))

                File.write("./c_src/hapi_functions_nif.c", entries)
                IO.puts("Generating c_src/hapi_functions_nif.c")
            end
        end

        # Helper function used to generate c stub entries.
        defp create_stub_c_entry(env, function_name, function_body, template_block) do
            String.replace(template_block, "%{HAPI_FUNCTION}%", function_name)
                |> String.replace("%{HAPI_FUNCTION_DOWNCASE}%", HAPI.Util.underscore(function_name))
        end
    end






























    # Given a function structure, return list of parameters which are used for return by pointer.
    defp function_get_return_parameters({_function_type, function_params}) do
        Enum.filter(function_params, &(function_check_return_parameter(&1)))
    end

    # Given a function structure, return parameters (ignore parameters returned by pointer).
    defp function_get_parameters({_function_type, function_params}) do
        Enum.filter(function_params, &(not function_check_return_parameter(&1)))
    end

    # Helper function to check if parameter is a return type parameter.
    defp function_check_return_parameter({_param_type, _param_name, dict}) do
        Dict.get(dict, :param_pointer, false) and not Dict.get(dict, :param_const, false)
    end

    # Helper function to check if parameter is a string.
    defp function_check_parameter_string({_param_type, _param_name, dict}) do
        Dict.get(dict, :param_string, false)
    end

    # Helper function to check if parameter is an array.
    defp function_check_parameter_array({_param_type, _param_name, dict}) do
        Dict.get(dict, :param_array, false)
    end


    # Helper method to get the name of the system.
    #defp get_os() do
    #    {family, name} = :os.type
    #    cond do
    #        name == :darwin ->
    #            :os_mac
    #        family == :win32 ->
    #            :os_win
    #        true ->
    #            raise(RuntimeError, description: "Unsupported platform")
    #            :os_unknown
    #    end
    #end















    # Generate c function stubs.
    def create_function_c_stubs(env) do
        IO.puts("Creating function c stubs in c_src/functions")

        funcs = Dict.get(env, :funcs, :nil)
        if not is_nil(funcs) do

            {:ok, template_function_c} = File.read("./util/hapi_function_nif.c.template")
            Enum.map(funcs, fn {k, v} -> create_function_c_stub(env, k, v, template_function_c) end)
        end

        env
    end

    # Generate function c stub.
    defp create_function_c_stub(env, function_name, {_return_type, parameters}, template_function_c) do

        {p_input, _p_process, _p_cleanup} = create_function_c_stub_objects(env, function_name, parameters)

        function_code = String.replace(template_function_c, "%{HAPI_FUNCTION}%", function_name)
            |> String.replace("%{HAPI_FUNCTION_DOWNCASE}%", HAPI.Util.underscore(function_name))

        # Create block for input parameters.
        param_decl = Enum.map_join(p_input, "\n    ", fn(x) -> x end)
        function_code = String.replace(function_code, "%{HAPI_FUNCTION_BODY}%", param_decl)

        file_name = "c_src/functions/#{HAPI.Util.underscore(function_name)}_nif.c"
        File.write("./#{file_name}", function_code)
        IO.puts("Generating #{file_name}")
    end

    # Create objects necessary for c stub function.
    defp create_function_c_stub_objects(env, fname, params) do
        create_function_c_stub_objects(env, fname, params, {[], [], []}, 0)
    end
    defp create_function_c_stub_objects(_env, _fname, [], ret, _idx) do
        ret
    end
    defp create_function_c_stub_objects(env, fname, [{:token_int, param_name, _opts} | rest], {i, p, c}, idx) do
        opt_i = "int32_t param_#{param_name} = 0;"
        opt_p = "!hapi_get_int(env, argv(#{Integer.to_string(idx)}), &param_#{param_name})";
        opt_c = :nil

        create_function_c_stub_objects(env, fname, rest, {i ++ [opt_i], p ++ [opt_p], c ++ [opt_c]}, idx + 1)
    end
    defp create_function_c_stub_objects(env, fname, [{:token_float, param_name, _opts} | rest], {i, p, c}, idx) do
        opt_i = "float param_#{param_name} = 0.0f;"
        opt_p = "!hapi_get_float(env, argv(#{Integer.to_string(idx)}), &param_#{param_name})";
        opt_c = :nil

        create_function_c_stub_objects(env, fname, rest, {i ++ [opt_i], p ++ [opt_p], c ++ [opt_c]}, idx + 1)
    end
    defp create_function_c_stub_objects(env, fname, [{:token_double, param_name, _opts} | rest], {i, p, c}, idx) do
        opt_i = "double param_#{param_name} = 0.0;"
        opt_p = "!hapi_get_double(env, argv(#{Integer.to_string(idx)}), &param_#{param_name})";
        opt_c = :nil

        create_function_c_stub_objects(env, fname, rest, {i ++ [opt_i], p ++ [opt_p], c ++ [opt_c]}, idx + 1)
    end
    defp create_function_c_stub_objects(env, fname, [{:token_bool, param_name, _opts} | rest], {i, p, c}, idx) do
        opt_i = "bool param_#{param_name} = false;"
        opt_p = "!hapi_get_bool(env, argv(#{Integer.to_string(idx)}), &param_#{param_name})";
        opt_c = :nil

        create_function_c_stub_objects(env, fname, rest, {i ++ [opt_i], p ++ [opt_p], c ++ [opt_c]}, idx + 1)
    end
    defp create_function_c_stub_objects(env, fname, [{"HAPI_Bool", param_name, opts} | rest], {i, p, c}, idx) do
        create_function_c_stub_objects(env, fname, [{:token_bool, param_name, opts} | rest], {i, p, c}, idx)
    end
    defp create_function_c_stub_objects(env, fname, [{param_type, param_name, opts} | rest], {i, p, c} = ret, idx) do
        cond do

            HAPI.Util.is_type_enum(env, param_type) or HAPI.Util.is_type_structure(env, param_type) ->
                param_type_underscore = HAPI.Util.underscore(param_type)
                opt_i = "#{param_type} param_#{param_name};"
                opt_p = "!hapi_get_#{param_type_underscore}(env, argv[#{Integer.to_string(idx)}], &param_#{param_name})"
                opt_c = :nil

                create_function_c_stub_objects(env, fname, rest, {i ++ [opt_i], p ++ [opt_p], c ++ [opt_c]}, idx + 1)

            #is_type_primitive(env, param_type) ->
            #    original_type = get_type_primitive(env, param_type)

                #opt_i = "#{original_type} param_#{param_name} = -1;"
                #opt_p = "!hapi_get_#{underscore(param_type)}(env, argv[#{Integer.to_string(idx)}], &param_#{param_name})"
                #opt_c = :nil
            true ->
                create_function_c_stub_objects(env, fname, rest, ret, idx + 1)
        end
    end
    defp create_function_c_stub_objects(env, fname, [_param | rest], ret, idx) do
        create_function_c_stub_objects(env, fname, rest, ret, idx + 1)
    end

    # Generate exports c stub containing NIF mapping table.
    defp create_exports_c_stub(funcs, template_exports_c) do
        exports = String.replace(template_exports_c, "%{HAPI_NIF_FUNCTIONS}%",
            Enum.map_join(funcs, ",\n    ", fn {k, v} ->
                create_exports_c_stub_entry(HAPI.Util.underscore(k), length(function_get_parameters(v))) end))

        File.write("./c_src/hapi_exports_nif.c", exports)
        IO.puts("Generating c_src/hapi_exports_nif.c")
    end

    # Helper function to create a NIF export table entry.
    defp create_exports_c_stub_entry("hapi_" <> rest, arity) do
        "{\"#{rest}\", #{Integer.to_string(arity)}, hapi_#{rest}}"
    end
end

[compiler, hapi_include_path] = System.argv()
HAPI.C.generate(compiler, hapi_include_path)
    |> HAPI.Lexical.parse()
    #|> HAPI.Lexical.print_tokens()
    |> HAPI.Syntactic.process()
    #|> HAPI.Syntactic.print_types()
    #|> HAPI.Syntactic.print_enums()
    #|> HAPI.Syntactic.print_structs()
    #|> HAPI.Syntactic.print_functions()
    |> HAPI.Types.create()
    |> HAPI.Enums.create()
    |> HAPI.Structures.create()
    |> HAPI.Functions.create()
