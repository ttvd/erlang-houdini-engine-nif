defmodule HAPI do

    # Remove preprocessor left overs from data stream.
    def preprocess(data) do
        String.replace(data, "int main() { return 0; }", "")
            |> String.replace(~r/#\s*\d+.*\n/, "")
            |> String.replace(~r/__attribute__\(\s*\(\s*visibility\(\s*\"default\"\s*\)\s*\)\s*\)\s+(\w+)/, "\\1")
            |> String.replace(~r/typedef\s+enum\s+\w+\s+\w+;/, "")
            |> String.replace(~r/typedef\s+struct\s+\w+\s+\w+;/, "")
    end

    # Parse given string containing code.
    def parse([]), do: []
    def parse(code), do: parse_collect(code, "", [])

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
    def print_tokens(tokens), do: Enum.map(tokens, fn(token) -> IO.inspect(token) end)

    # Given a list of tokens, produce a mapping table from hapi types.
    def type_map_hapi(tokens) do
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
    def print_type_map_hapi(dict), do: Enum.map(dict, fn {k, v} -> IO.puts("#{k} -> #{v}") end)

    # Given a list of tokens, produce a mapping table of hapi enums.
    def enum_map_hapi(tokens), do: HashDict.new |> enum_map_hapi_collect(tokens)

    # Process tokens and collect enums.
    defp enum_map_hapi_collect(dict, []), do: dict
    defp enum_map_hapi_collect(dict, [:token_enum, enum_name, :token_bracket_curly_left | rest]) do
        {enum_dict, remaining} = enum_map_hapi_extract(HashDict.new, rest, 0)
        Dict.put(dict, enum_name, enum_dict) |> enum_map_hapi_collect(remaining)
    end
    defp enum_map_hapi_collect(_dict, [:token_enum | _rest]), do: raise(SyntaxError, description: "Invalid enum detected")
    defp enum_map_hapi_collect(dict, [_token | rest]), do: enum_map_hapi_collect(dict, rest)

    # Helper function to extract enum values from token stream.
    defp enum_map_hapi_extract(_dict, [], _idx), do: raise(SyntaxError, description: "Malformed enum detected")
    defp enum_map_hapi_extract(dict, [:token_comma | rest], idx), do: enum_map_hapi_extract(dict, rest, idx)
    defp enum_map_hapi_extract(dict, [:token_bracket_curly_right, :token_semicolon | rest], _idx), do: {dict, rest}
    defp enum_map_hapi_extract(dict, [enum_entry, :token_comma | rest], idx) do
        Dict.put(dict, enum_entry, idx) |> enum_map_hapi_extract(rest, idx + 1)
    end
    defp enum_map_hapi_extract(dict, [enum_entry, :token_bracket_curly_right, :token_semicolon | rest], idx) do
        {Dict.put(dict, enum_entry, idx), rest}
    end
    defp enum_map_hapi_extract(dict, [enum_entry, :token_assignment, enum_value | rest], _idx) when is_integer(enum_value) do
        Dict.put(dict, enum_entry, enum_value) |> enum_map_hapi_extract(rest, enum_value + 1)
    end
    defp enum_map_hapi_extract(dict, [enum_entry, :token_assignment, enum_value | rest], _idx) do
        (&(enum_map_hapi_extract(Dict.put(dict, enum_entry, &1), rest, &1 + 1))).(enum_map_hapi_lookup_value(dict, enum_value))
    end

    # Helper function used to look up enum value within enum table.
    defp enum_map_hapi_lookup_value(dict, enum_value) do
        if Dict.has_key?(dict, enum_value) do
            orig_value = Dict.get(dict, enum_value)
            if is_integer(orig_value) do
                orig_value
            else
                enum_map_hapi_lookup_value(dict, orig_value)
            end
        else
            raise(SyntaxError, description: "Unknown enum value assignment #{enum_value}")
        end
    end

    # Print from hapi enums dictionary.
    def print_enum_map_hapi(dict), do: Enum.map(dict, fn {k, v} -> print_enum_map_hapi(k, v) end)

    # Helper function to print each enum.
    defp print_enum_map_hapi(enum_name, enum_dict) do
        IO.puts("#{enum_name}")
        Enum.map(enum_dict, fn {k, v} -> IO.puts("    #{k} -> #{v}") end)
        IO.puts("")
    end

    # Given a list of tokens, produce a mapping table of structures.
    def struct_map_hapi(tokens), do: HashDict.new |> struct_map_hapi_collect(tokens)

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
    defp struct_map_hapi_extract(_list, []), do: raise(SyntaxError, description: "Malformed struct detected")
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
    def print_struct_map_hapi(dict), do: Enum.map(dict, fn {k, v} -> print_struct_map_hapi(k, v) end)

    # Helper function to print each struct.
    defp print_struct_map_hapi(struct_name, struct_body) do
        IO.puts("#{struct_name}")
        Enum.map(struct_body, fn(field) -> print_struct_map_hapi_field(field) end)
        IO.puts("")
    end

    # Helper function to print each struct field.
    defp print_struct_map_hapi_field([field_name, field_type, field_size]) do
        IO.puts("    #{field_type} #{field_name}[#{field_size}]")
    end
    defp print_struct_map_hapi_field([field_name, field_type]) do
        IO.puts("    #{field_type} #{field_name}")
    end
end

{:ok, data} = File.read("hapi.c.generated.osx")
data = HAPI.preprocess(data)
data = HAPI.parse(data)
#HAPI.print_tokens(data)

types_from_hapi = HAPI.type_map_hapi(data)
#HAPI.print_type_map_hapi(types_from_hapi)

types_enums_from_hapi = HAPI.enum_map_hapi(data)
#HAPI.print_enum_map_hapi(types_enums_from_hapi)

types_structs_from_hapi = HAPI.struct_map_hapi(data)
HAPI.print_struct_map_hapi(types_structs_from_hapi)
