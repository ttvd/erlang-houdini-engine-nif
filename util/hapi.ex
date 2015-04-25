defmodule HAPI do

    # Remove preprocessor left overs from data stream.
    defp preprocess(data) do
        String.replace(data, ~r/int main\(\)\s\{\s*.*\s*\}/, "")
            |> String.replace(~r/#\s*\d+.*\n/, "")
            |> String.replace(~r/__attribute__\(\s*\(\s*visibility\(\s*\"default\"\s*\)\s*\)\s*\)\s+(\w+)/, "\\1")
            |> String.replace("__attribute__((visibility(0)))", "")
            |> String.replace(~r/typedef\s+enum\s+\w+\s+\w+;/, "")
            |> String.replace(~r/typedef\s+struct\s+\w+\s+\w+;/, "")
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
    defp map_token("struct"), do: [:token_struct]
    defp map_token("const"), do: [:token_const]
    defp map_token("void"), do: [:token_void]
    defp map_token("float"), do: [:token_float]
    defp map_token("int"), do: [:token_int]
    defp map_token("char"), do: [:token_char]
    defp map_token(token) do
        case Integer.parse(token) do
            {num, ""} ->
                [num]
            _ ->
                [token]
        end
    end

    # Print tokens.
    #defp print_tokens(tokens), do: Enum.map(tokens, fn(token) -> IO.inspect(token) end)

    # Given a list of tokens, produce a mapping table from hapi types.
    defp type_map_hapi(tokens) do
        HashDict.new
            |> Dict.put("void", :token_void)
            |> Dict.put("int", :token_int)
            |> Dict.put("float", :token_float)
            |> Dict.put("bool", :token_bool)
            |> type_map_hapi_collect(tokens)
    end

    # Process tokens and collect types.
    defp type_map_hapi_collect(dict, []), do: dict
    defp type_map_hapi_collect(dict, [:token_typedecl, _type_origin, "HAPI_Bool" | rest]) do
        Dict.put(dict, "HAPI_Bool", :token_bool) |> type_map_hapi_collect(rest)
    end
    defp type_map_hapi_collect(dict, [:token_typedecl, type_origin, type_new | rest]) do
        Dict.put(dict, type_new, type_origin) |> type_map_hapi_collect(rest)
    end
    defp type_map_hapi_collect(dict, [:token_enum, enum_name | rest]) do
        Dict.put(dict, enum_name, :token_enum) |> type_map_hapi_collect(rest)
    end
    defp type_map_hapi_collect(dict, [:token_struct, struct_name | rest]) do
        Dict.put(dict, struct_name, :token_struct) |> type_map_hapi_collect(rest)
    end
    defp type_map_hapi_collect(dict, [_token | rest]), do: type_map_hapi_collect(dict, rest)

    # Print from hapi type dictionary.
    #defp print_type_map_hapi(dict), do: Enum.map(dict, fn {k, v} -> IO.puts("#{k} -> #{v}") end)

    # Given a list of tokens, produce a mapping table of hapi enums.
    defp enum_map_hapi(tokens), do: HashDict.new |> enum_map_hapi_collect(tokens)

    # Process tokens and collect enums.
    defp enum_map_hapi_collect(dict, []), do: dict
    defp enum_map_hapi_collect(dict, [:token_enum, enum_name, :token_bracket_curly_left | rest]) do
        {enum_values, remaining} = enum_map_hapi_extract([], rest, 0)
        Dict.put(dict, enum_name, enum_values) |> enum_map_hapi_collect(remaining)
    end
    defp enum_map_hapi_collect(_dict, [:token_enum | _rest]), do: raise(SyntaxError, description: "Invalid enum detected")
    defp enum_map_hapi_collect(dict, [_token | rest]), do: enum_map_hapi_collect(dict, rest)

    # Helper function to extract enum values from token stream.
    defp enum_map_hapi_extract(_values, [], _idx), do: raise(SyntaxError, description: "Unexpected end of enum")
    defp enum_map_hapi_extract(values, [:token_comma | rest], idx), do: enum_map_hapi_extract(values, rest, idx)
    defp enum_map_hapi_extract(values, [:token_bracket_curly_right, :token_semicolon | rest], _idx), do: {values, rest}
    defp enum_map_hapi_extract(values, [enum_entry, :token_comma | rest], idx) do
        values ++ [{enum_entry, idx}] |> enum_map_hapi_extract(rest, idx + 1)
    end
    defp enum_map_hapi_extract(values, [enum_entry, :token_bracket_curly_right, :token_semicolon | rest], idx) do
        {values ++ [{enum_entry, idx}], rest}
    end
    defp enum_map_hapi_extract(values, [enum_entry, :token_assignment, enum_value | rest], _idx) when is_integer(enum_value) do
        values ++ [{enum_entry, enum_value}] |> enum_map_hapi_extract(rest, enum_value + 1)
    end
    defp enum_map_hapi_extract(values, [enum_entry, :token_assignment, enum_value | rest], _idx) do
        orig_value = enum_map_hapi_lookup_value(values, values, enum_value)
        values ++ [{enum_entry, orig_value, enum_value}] |> enum_map_hapi_extract(rest, orig_value + 1)
    end

    # Helper function used to look up enum value within enum table.
    defp enum_map_hapi_lookup_value([], _values, _enum_value), do: raise(SyntaxError, description: "Enum value was not found")
    defp enum_map_hapi_lookup_value([field | rest], values, enum_value) do
        if elem(field, 0) == enum_value do
            field_value = elem(field, 1)
            if is_integer(field_value) do
                field_value
            else
                enum_map_hapi_lookup_value(values, values, field_value)
            end
        else
            enum_map_hapi_lookup_value(rest, values, enum_value)
        end
    end

    # Print from hapi enums dictionary.
    defp print_enum_map_hapi(dict), do: Enum.map(dict, fn {k, v} -> print_enum_map_hapi(k, v) end)

    # Helper function to print each enum body.
    defp print_enum_map_hapi(enum_name, enum_values) do
        IO.puts("#{enum_name}")
        Enum.map(enum_values, fn(x) -> print_enum_map_hapi_field(x) end)
        IO.puts("")
    end

    #Helper function to print each enum.
    defp print_enum_map_hapi_field({field_name, field_value, field_original}) do
        IO.puts("    #{field_name} -> #{field_original} -> #{field_value}")
    end
    defp print_enum_map_hapi_field({field_name, field_value}), do: IO.puts("    #{field_name} -> #{field_value}")

    # Given a list of tokens, produce a mapping table of structures.
    defp struct_map_hapi(tokens), do: HashDict.new |> struct_map_hapi_collect(tokens)

    # Process tokens and collect structures.
    defp struct_map_hapi_collect(dict, []), do: dict
    defp struct_map_hapi_collect(dict, [:token_struct, struct_name, :token_bracket_curly_left | rest]) do
        [struct_body, remaining] = struct_map_hapi_extract([], rest)
        Dict.put(dict, struct_name, struct_body) |> struct_map_hapi_collect(remaining)
    end
    defp struct_map_hapi_collect(_dict, [:token_struct | _rest]) do
        raise(SyntaxError, description: "Invalid struct detected")
    end
    defp struct_map_hapi_collect(dict, [_token | rest]), do: struct_map_hapi_collect(dict, rest)

    # Helper function to extract struct fields from token stream.
    defp struct_map_hapi_extract(_list, []), do: raise(SyntaxError, description: "Unexpected end of struct")
    defp struct_map_hapi_extract(list, [:token_bracket_curly_right, :token_semicolon | rest]) do
        [list, rest]
    end
    defp struct_map_hapi_extract(list, [field_type, field_name, :token_bracket_square_left, field_size,
        :token_bracket_square_right, :token_semicolon | rest]) do
            list ++ [[field_name, field_type, field_size]] |> struct_map_hapi_extract(rest)
    end
    defp struct_map_hapi_extract(list, [field_type, field_name, :token_semicolon | rest]) do
        list ++ [[field_name, field_type]] |> struct_map_hapi_extract(rest)
    end

    # Print from hapi structs dictionary.
    #defp print_struct_map_hapi(dict), do: Enum.map(dict, fn {k, v} -> print_struct_map_hapi(k, v) end)

    # Helper function to print each struct.
    #defp print_struct_map_hapi(struct_name, struct_body) do
    #    IO.puts("#{struct_name}")
    #    Enum.map(struct_body, fn(field) -> print_struct_map_hapi_field(field) end)
    #    IO.puts("")
    #end

    # Helper function to print each struct field.
    #defp print_struct_map_hapi_field([field_name, field_type, field_size]) do
    #    IO.puts("    #{field_type} #{field_name}[#{field_size}]")
    #end
    #defp print_struct_map_hapi_field([field_name, field_type]) do
    #    IO.puts("    #{field_type} #{field_name}")
    #end

    # Given a list of tokens, produce a mapping table of functions.
    defp function_map_hapi(tokens), do: HashDict.new |> function_map_hapi_collect(tokens)

    # Process tokens and collect functions.
    defp function_map_hapi_collect(dict, []), do: dict
    defp function_map_hapi_collect(dict, [function_type, function_name, :token_bracket_left | rest]) do
        [params, remaining] = function_map_hapi_extract_params([], rest)
        Dict.put(dict, function_name, [function_type, params]) |> function_map_hapi_collect(remaining)
    end
    defp function_map_hapi_collect(dict, [_token | rest]), do: function_map_hapi_collect(dict, rest)

    # Helper function to extract function parameters.
    defp function_map_hapi_extract_params(_list, []), do: raise(SyntaxError, description: "Unexpected end of function")
    defp function_map_hapi_extract_params(list, [:token_comma | rest]), do: function_map_hapi_extract_params(list, rest)
    defp function_map_hapi_extract_params(list, [:token_bracket_right, :token_semicolon | rest]) do
        [list, rest]
    end
    defp function_map_hapi_extract_params(list, [:token_const, param_type, :token_pointer, param_name | rest] = tokens) do
        list ++ [[param_type, param_name, HashDict.new |> function_map_hapi_param_flags(Enum.take(tokens, 4))]]
            |> function_map_hapi_extract_params(rest)
    end
    defp function_map_hapi_extract_params(list, [:token_const, param_type, param_name | rest] = tokens) do
        list ++ [[param_type, param_name, HashDict.new |> function_map_hapi_param_flags(Enum.take(tokens, 3))]]
            |> function_map_hapi_extract_params(rest)
    end
    defp function_map_hapi_extract_params(list, [param_type, :token_pointer, param_name | rest] = tokens) do
        list ++ [[param_type, param_name, HashDict.new |> function_map_hapi_param_flags(Enum.take(tokens, 3))]]
            |> function_map_hapi_extract_params(rest)
    end
    defp function_map_hapi_extract_params(list, [param_type, param_name | rest] = tokens) do
        list ++ [[param_type, param_name, HashDict.new |> function_map_hapi_param_flags(Enum.take(tokens, 2))]]
            |> function_map_hapi_extract_params(rest)
    end

    # Helper function used add flags.
    defp function_map_hapi_param_flags(dict, []), do: dict
    defp function_map_hapi_param_flags(dict, [param | rest]) do
        case param do
            :token_const ->
                Dict.put(dict, :param_const, true) |> function_map_hapi_param_flags(rest)
            :token_pointer ->
                Dict.put(dict, :param_pointer, true) |> function_map_hapi_param_flags(rest)
            _ ->
                if is_binary(param) do
                    if String.match?(param, ~r/\w+es$/) or String.match?(param, ~r/\w+s$/) do
                        Dict.put(dict, :param_array, true) |> function_map_hapi_param_flags(rest)
                    else
                        function_map_hapi_param_flags(dict, rest)
                    end
                else
                    function_map_hapi_param_flags(dict, rest)
                end
        end
    end

    # Given a function structure, return list of parameters which are used for return by pointer.
    defp function_get_return_parameters([_function_type, function_params]) do
        Enum.filter(function_params, &(function_check_return_parameter(&1)))
    end

    # Helper function to check if parameter is a return type parameter.
    defp function_check_return_parameter([_param_type, _param_name, dict]) do
        Dict.get(dict, :param_pointer, false) and not Dict.get(dict, :param_const, false)
    end

    # Print from hapi functions dictionary.
    #defp print_function_map_hapi(dict), do: Enum.map(dict, fn {k, v} -> print_function_map_hapi(k, v) end)

    # Helper function to print each function.
    #defp print_function_map_hapi(function_name, [function_type, function_params]) do
    #    IO.puts("#{function_name} -> #{function_type}")
    #    Enum.map(function_params, fn(param) -> print_function_map_hapi_param(param) end)
    #    IO.puts("")
    #end

    # Helper function to print each param.
    #defp print_function_map_hapi_param([param_type, param_name]) do
    #    IO.puts("    #{param_type} #{param_name}")
    #end
    #defp print_function_map_hapi_param([param_type, param_name, param_opts]) do
    #    IO.puts("    #{param_type} #{param_name}")
    #    Enum.map(param_opts, fn {k, v} -> IO.puts("        #{k} -> #{v}") end)
    #end

    # Create environment consisting of types, enums, structs and functions.
    defp parse(data) do
        tokens = preprocess(data) |> tokenize()
        HashDict.new |>
            Dict.put(:types, type_map_hapi(tokens)) |>
            Dict.put(:enums, enum_map_hapi(tokens)) |>
            Dict.put(:structs, struct_map_hapi(tokens)) |>
            Dict.put(:funcs, function_map_hapi(tokens))
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

    # Helper function to generate hash for a given string.
    defp hash_binary(string) do
        if File.exists?("./util/xxhash") do
            {cmd_output, result_code} = System.cmd("./util/xxhash", [])
            if 0 == result_code do
                cmd_output
            else
                raise(RuntimeError, description: "error executing xxhash utility")
            end
        else
            raise(RuntimeError, description: "xxhash utility was not compiled and is missing")
        end
    end

    # Pre-process hapi.c which includes all hapi headers into something we can parse.
    def generate_hapi_c("clang", hapi_include_path) do
        parse_compile("clang", ["-cc1", "-ast-print", "-I#{hapi_include_path}", "./util/hapi.c"])
    end
    def generate_hapi_c("cpp.exe", hapi_include_path) do
        parse_compile("./util/cpp.exe", ["-E", "-I\"#{hapi_include_path}\"", "./util/hapi.c"])
    end
    def generate_hapi_c(_compiler, _hapi_include_path) do
        raise(RuntimeError, description: "Unknown compiler, please add options")
    end

    # Generate enum c stubs containing c <-> erl convertors.
    defp create_enum_c_stubs(env) do
        enums = Dict.get(env, :enums, :nil)
        if not is_nil(enums) do
            {:ok, template} = File.read("./util/hapi_enum_nif.c.template")
            {:ok, template_c_to_erl} = File.read("./util/hapi_enum_nif.c.c_to_erl.template")
            {:ok, termplate_erl_to_c} = File.read("./util/hapi_enum_nif.c.erl_to_c.template")

            Enum.map(enums, fn {k, v} -> create_enum_c_stub(k, v, template, template_c_to_erl, termplate_erl_to_c) end)
        end
    end

    # Function used to generate c stub containing c <-> erl C conversion functions.
    defp create_enum_c_stub(enum_name, enum_body, template, template_c_to_erl, termplate_erl_to_c) do



        c_to_erl_blocks = Enum.map(enum_body, fn(f) -> create_enum_c_stub_c_to_erl_block(template_c_to_erl, f) end) |>
            Enum.filter(fn(f) -> not is_nil(f) end)

        enum_name_downcase = String.downcase(enum_name)
        enum_code = template
            |> String.replace("%{HAPI_ENUM}%", enum_name)
            |> String.replace("%{HAPI_ENUM_DOWNCASE}%", enum_name_downcase)
            |> String.replace("%{HAPI_ENUM_C_TO_ERL_BODY}%", Enum.join(c_to_erl_blocks, "\n"))

        File.write("./c_src/enums/#{String.downcase(enum_name)}_nif.c", enum_code)
    end

    # Function to generate c_to_erl block for c <-> erl enum c stub.
    defp create_enum_c_stub_c_to_erl_block(template_c_to_erl, {field_name, field_value, field_original}), do: :nil
    defp create_enum_c_stub_c_to_erl_block(template_c_to_erl, {field_name, field_value}) do
        [String.replace(template_c_to_erl, "%{HAPI_ENUM_VALUE}%", field_name)
            |> String.replace("%{HAPI_ENUM_VALUE_DOWNCASE}%", String.downcase(field_name))]
    end

    # Helper function to compile and save output into generated file.
    defp parse_compile(compiler, compiler_flags) do
        {cmd_output, result_code} = System.cmd(compiler, compiler_flags)
        if 0 == result_code do
            parse(cmd_output)
                |> create_enum_c_stubs()
        else
            raise(RuntimeError, description: "Unable to expand macros in hapi.c")
        end
    end
end

[compiler, hapi_include_path] = System.argv()
HAPI.generate_hapi_c(compiler, hapi_include_path)
