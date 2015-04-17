require 'rake'
require 'rubygems'

begin
    require 'xxhash'
rescue LoadError
    puts "Please install xxhash Ruby gem."
    exit
end

# Helper function to produce underscore version of a string.
def create_underscore(string)
    if string.match(/\A[A-Z]+\z/)
        string.downcase
    else
        result = string.gsub(/([A-Z]+)([A-Z][a-z])/, '\1_\2')
        result = result.gsub(/([a-z])([A-Z])/, '\1_\2')
        result.downcase
    end
end

# Helper function to produce camelcase version of underscore string.
def create_camelcase(string)
    string.split("_").each { |s| s.capitalize! }.join("")
end

# Helper function to generate Erlang to C mapping function signature.
def generate_enum_erl_to_c_function(enum_name_underscore, enum_name)

    "bool hapi_enum_#{enum_name_underscore}_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_#{enum_name}* #{enum_name_underscore})"
end

# Helper function to generate C to Erlang mapping function signature.
def generate_enum_c_to_erl_function(enum_name_underscore, enum_name)

    "ERL_NIF_TERM hapi_enum_#{enum_name_underscore}_c_to_erl(ErlNifEnv* env, HAPI_#{enum_name} #{enum_name_underscore})"
end

# Helper function to generate Erlang to C mapping for a given enum.
def generate_enum_erl_to_c_body(enum_name, enum_value_tuples)

    # Buffer to hold if / if else entries.
    erl_to_c_buffer = []

    enum_value_tuples.each_with_index do |value_tuple, index|

        name_entry = "hapi_#{value_tuple[0].downcase}"
        name_hash = XXhash.xxh32(name_entry, 0)

        entry = ""
        entry << "    // \"#{name_entry}\"#{$/}"
        entry << "            case #{name_hash}:#{$/}"
        entry << "            {#{$/}"
        entry << "                *#{enum_name} = HAPI_#{value_tuple[0]};#{$/}"
        entry << "            }#{$/}"

        erl_to_c_buffer << entry
    end

    erl_to_c_buffer.join "#{$/}        "
end

# Helper function to generate C to Erlang mapping for a given enum.
def generate_enum_c_to_erl_body(enum_value_tuples)

    # Buffer to hold case entries.
    c_to_erl_buffer = []

    # Hash to detect duplicates.
    map_table = {}

    enum_value_tuples.each do |value_tuple|

        valid_entry = true
        case_entry = ""

        if not value_tuple[1].empty?
            if not value_tuple[1] =~ /\A\d+\z/
                valid_entry = false
            end
        end

        case_entry << "/*#{$/}        " if not valid_entry
        case_entry << "case HAPI_#{value_tuple[0]}:#{$/}        {#{$/}            return hapi_private_make_hash_tuple(env, \"hapi_#{value_tuple[0].downcase}\");#{$/}        }"
        case_entry << "#{$/}"
        case_entry << "        */#{$/}" if not valid_entry

        c_to_erl_buffer << case_entry
    end

    c_to_erl_buffer.join "#{$/}        "
end

# Helper function to generate enum nif files.
def generate_nif_enums(hapi_common_header)

    # Scan enum tuples from the common header.
    scanned_enums = scan_enums(hapi_common_header)

    # Store enum source file names.
    enum_source_filenames = []

    # Store conversion function signatures.
    enum_conversion_functions = []

    # We need to further process enum values.
    scanned_enums.each do |enum_entry|

        enum_name = enum_entry[0]
        enum_body = enum_entry[1]

        # Get lowercase underscore'd version of enum name.
        enum_name_underscore = create_underscore(enum_name)

        # Remove any comments from body.
        enum_body.gsub!(/\s*\/\/\/.*$/, "")
        enum_body.gsub!(/^\s*\/\/\/.*$/, "")
        enum_body.gsub!(/^\s*\/\/.*$/, "")
        enum_body.gsub!(/$\s*$/, "")

        # Read template enum file.
        template_enum_nif_file = File.read "./c_src/hapi_enum_nif.c.template"

        # Replace enum lower case name in template file.
        template_enum_nif_file.gsub!("%{HAPI_ENUM_NAME_L}", enum_name_underscore)

        # Replace capitalized enum name in tmeplate file.
        template_enum_nif_file.gsub!("%{HAPI_ENUM_NAME_C}", enum_name)

        # Create function signatures.
        function_erl_to_c = generate_enum_erl_to_c_function(enum_name_underscore, enum_name)
        function_c_to_erl = generate_enum_c_to_erl_function(enum_name_underscore, enum_name)
        enum_conversion_functions << [function_erl_to_c, function_c_to_erl]

        # Replace function signatures.
        template_enum_nif_file.gsub!("%{HAPI_ENUM_ERL_TO_C_FUNCTION}%", function_erl_to_c)
        template_enum_nif_file.gsub!("%{HAPI_ENUM_C_TO_ERL_FUNCTION}%", function_c_to_erl)

        # Split enum values.
        enum_values_raw = enum_body.split ","

        # Store enum value tuples.
        enum_value_tuples = []

        enum_values_raw.each do |enum_value_raw|

            enum_value = enum_value_raw.strip
            enum_value_scan = enum_value.scan(/^HAPI_([^\s]*)\s*\=\s*([^\s]*)$/)

            enum_entry = ""
            enum_entry_value = ""

            if not enum_value_scan.empty?

                enum_entry = enum_value_scan[0][0]
                enum_entry_value = enum_value_scan[0][1].gsub("HAPI_", "")
            else

                enum_entry = enum_value.gsub("HAPI_", "")
            end

            enum_value_tuples << [enum_entry, enum_entry_value]
        end

        # Replace generate C to Erlang block.
        template_enum_nif_file.gsub!("%{HAPI_ENUM_C_TO_ERL_BODY}%", generate_enum_c_to_erl_body(enum_value_tuples))

        template_enum_nif_file.gsub!("%{HAPI_ENUM_ERL_TO_C_BODY}%", generate_enum_erl_to_c_body(enum_name_underscore,
            enum_value_tuples))

        # Create source filename for this enum.
        enum_source_filename = "hapi_enum_#{enum_name_underscore}_nif.c"

        # Write out the file.
        File.open("./c_src/#{enum_source_filename}", 'w') do |file|

            # Store filename for this enum.
            enum_source_filenames << enum_source_filename

            # Write result.
            file.write(template_enum_nif_file)
        end
    end

    # Read template header file.
    template_enums_nif_file = File.read "./c_src/hapi_enums_nif.h.template"

    # Store signature entries.
    enum_function_signatures = []

    # Write function signatures to header file.
    enum_conversion_functions.each_with_index do |conversion_functions, index|

        signature_block = "// From #{enum_source_filenames[index]}#{$/}"
        signature_block << "#{conversion_functions[0]};#{$/}"
        signature_block << "#{conversion_functions[1]};"

        enum_function_signatures << signature_block
    end

    File.open("./c_src/hapi_enums_nif.h", 'w') do |file|

        template_enums_nif_file.gsub!("%{HAPI_ENUM_CONVERSION_FUNCTIONS}%", enum_function_signatures.join("#{$/}#{$/}"))
        file.write(template_enums_nif_file)
    end
end

# Helper function to generate nif c stubs for HAPI enums.
def scan_enums(hapi_common_header)

    # Read hapi common header.
    hapi_common_file = File.read hapi_common_header

    # Preprocess for regex matching.
    hapi_common_file.gsub!("@{", "").gsub!("@}", "")

    # Extract all enum entries.
    hapi_enum_entries = hapi_common_file.scan /^enum\s+HAPI_([^\s]*)\s*$\s*{([^\}]*)\s*$\s*\};/i

    # Create array to hold tuples.
    scanned_enums = []

    hapi_enum_entries.each do |entry|
        scanned_enums <<  [entry[0], entry[1]]
    end

    # Return scanned tuples.
    scanned_enums
end

# Helper function to generate hapi_functions_nif_stubs.c.generated
def generate_hapi_functions_nif_stubs_c_generated(buffer_function_names_original, buffer_function_names)

    # Buffer to hold generated stubs.
    generated_stubs = []

    buffer_function_names_original.each_with_index do |entry, index|

        stub_entry = ""
        stub_entry << "// HAPI_#{entry} equivalent.#{$/}"
        stub_entry << "ERL_NIF_TERM#{$/}"
        stub_entry << "hapi_#{buffer_function_names[index]}_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])#{$/}"
        stub_entry << "{#{$/}"
        stub_entry << "    // Needs implementation.#{$/}"
        stub_entry << "    return hapi_enum_result_c_to_erl(env, HAPI_RESULT_SUCCESS);#{$/}"
        stub_entry << "}#{$/}"

        generated_stubs << stub_entry
    end

    File.open("./c_src/hapi_functions_nif_stubs.c.generated", 'w') do |file|
        file.write(generated_stubs.join("#{$/}"))
    end
end

# Helper function to generate hapi_functions_nif.h
def generate_hapi_functions_nif_h(function_names, buffer_arities)

    # Read template header and source file.
    template_file_header = File.read "./c_src/hapi_functions_nif.h.template"
    template_file_source = File.read "./c_src/hapi_functions_nif.c.template"

    # Buffer to store prototypes.
    function_prototypes = []

    # Buffer to store mappings.
    function_mappings = []

    function_names.each_with_index do |function_name, index|

        function_prototypes << "extern ERL_NIF_TERM hapi_#{function_name}_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);"
        function_mappings << "    {\"#{function_name}\", #{buffer_arities[index]}, hapi_#{function_name}_impl}"
    end

    # Replace prototypes entry.
    template_file_header.gsub!("%{HAPI_FUNCTION_PROTOTYPES}%", function_prototypes.join("#{$/}"))

    # Replace mappings entry.
    template_file_source.gsub!("%{HAPI_FUNCTION_MAPPING}%", function_mappings.join(",#{$/}"))

    # Write out the files.
    File.open("./c_src/hapi_functions_nif.h", 'w') do |file|
        file.write(template_file_header)
    end

    File.open("./c_src/hapi_functions_nif.c", 'w') do |file|
        file.write(template_file_source)
    end
end

# Helper function to generate hapi.erl
def generate_hapi_erl(buffer_exports, buffer_functions, buffer_ignored_params)

    # Read template file.
    template_file = File.read "./src/hapi.erl.template"

    # Replace export template entry.
    template_file.gsub!("%{HAPI_EXPORT_FUNCTIONS}%", buffer_exports.join(",#{$/}"))

    # Replace function template entry.
    text_block_functions = []

    buffer_functions.each_with_index do |entry, index|

        block_entry = "%#{$/}"

        # Get ignored parameters for this entry.
        ignored_parameters = buffer_ignored_params[index]

        # Get spec for this function.
        result = "hapi_result()"

        if ignored_parameters.length > 0
            block_entry << "%-spec #{entry} -> {#{result}, #{ignored_parameters.join(', ')}}."
        else
            block_entry << "%-spec #{entry} -> #{result}."
        end

        # Add function entry to block.
        block_entry << "#{$/}#{entry} ->#{$/}    ?nif_stub."

        text_block_functions << block_entry
    end

    template_file.gsub!("%{HAPI_IMPL_FUNCTIONS}%", text_block_functions.join("#{$/}#{$/}"))

    # Write out the file.
    File.open("./src/hapi.erl", 'w') do |file|

        file.write(template_file)
    end
end

# Helper function to create hapi.erl and other resources from template.
def generate_hapi_erl_and_functions(hapi_header)

    # Read hapi header.
    hapi_file = File.read hapi_header

    # Create buffer for exports.
    buffer_exports = []

    # Create buffer for functions.
    buffer_functions = []

    # Create buffer for function specs.
    buffer_function_specs = []

    # Ignored parameters.
    buffer_ignored_params = []

    # Create buffer to hold function names.
    buffer_function_names = []
    buffer_function_names_original = []

    # Create buffer to hold arities.
    buffer_arities = []

    # Extract all hapi entries.
    hapi_entries = hapi_file.scan /HAPI_DECL\s+HAPI_([^(]*)\(([^)]*)\);/i

    hapi_entries.each do |entry|

        hapi_entry_name = "#{create_underscore entry[0]}"
        hapi_entry_param_string = entry[1].gsub(/(\S)[^\S\n]*\n[^\S\n]*(\S)/, '\1 \2').strip
        hapi_entry_params = hapi_entry_param_string.split(",")

        # Store function names.
        buffer_function_names_original << entry[0]
        buffer_function_names << hapi_entry_name

        # Store processed parameters.
        processed_params = []

        # Ignored parameters.
        ignored_parameters = []

        # We need to skip output parameters.
        hapi_entry_params.each do |parm|

            processed_param = parm.strip
            processed_param_name = "_#{create_camelcase(processed_param.split(' ').last)}"

            if not processed_param.start_with? "const" and processed_param.strip.include? " * "
                ignored_parameters << processed_param_name.gsub("_*", "_")
                next
            else
                processed_params << processed_param_name.gsub("_*", "_")
            end
        end

        # Store arity.
        buffer_arities << processed_params.length

        # Store ignored parameters.
        buffer_ignored_params << ignored_parameters

        # Add function name and arity to export table.
        buffer_exports << "    #{hapi_entry_name}/#{processed_params.length}"

        # Create function spec entry.
        buffer_function_spec = "-spec "
        buffer_function_spec << "#{hapi_entry_name}("
        buffer_function_spec << ") -> "
        buffer_function_spec << "hapi_result()."

        buffer_function_specs << buffer_function_spec

        # Create function entry.
        buffer_functions << "#{hapi_entry_name}(#{processed_params.join(", ")})"
    end

    # Generate hapi.erl
    generate_hapi_erl(buffer_exports, buffer_functions, buffer_ignored_params)

    # Generate hapi_functions_nif.h
    generate_hapi_functions_nif_h(buffer_function_names, buffer_arities)

    # Generate hapi nif function c stubs for easier copying.
    generate_hapi_functions_nif_stubs_c_generated(buffer_function_names_original, buffer_function_names)
end


# Set default task.
task :default => [:help]

#
desc "Help"
task :help do

    puts %x{rake --tasks}
end

namespace :erlang do

    # This will generate hapi.erl
    desc "Generate hapi.erl from HAPI.h"
    task :generate_erl_from, [:hapi_header_path] do |t, args|
        hapi_path = args.values_at(:hapi_header_path)

        if not hapi_path.first.nil?
            hapi_path = hapi_path.first

            if not File.file? hapi_path
                hapi_path = "#{hapi_path}/HAPI.h"
            end

            if File.file? hapi_path
                generate_hapi_erl_and_functions hapi_path
            else
                puts "Could not locate HAPI.h"
            end
        else
            puts "Please provide location of HAPI.h as parameter."
        end
    end

    # This will attempt to detect where HAPI.h is and run generation.
    desc "Locate HAPI_Common.h and generate hapi.erl from it"
    task :generate_erl do

        if RUBY_PLATFORM =~ /^.*darwin.*$/

            hapi_common_header = "/Library/Frameworks/Houdini.framework/Resources/toolkit/include/HAPI/HAPI.h"

            if File.exists? hapi_common_header
                Rake::Task["erlang:generate_erl_from"].invoke hapi_common_header
            end
        end
    end

    # This will generate hapi enum stubs.
    desc "Generate hapi enum stubs from HAPI_Common.h"
    task :generate_enums_from, [:hapi_common_header_path] do |t, args|
        hapi_path = args.values_at(:hapi_common_header_path)

        if not hapi_path.first.nil?
            hapi_path = hapi_path.first

            if File.file? hapi_path
                generate_nif_enums hapi_path
            else
                hapi_path = "#{hapi_path}/HAPI_Common.h"

                if File.file? hapi_path
                    generate_nif_enums hapi_path
                else
                    puts "Could not locate HAPI_Common.h"
                end
            end
        else
            puts "Please provide location of HAPI_Common.h as parameter."
        end
    end

    # This will attempt to detect where HAPI_Common.h is and run generation.
    desc "Locate HAPI_Common.h and generate hapi enum stubs from it"
    task :generate_enums do

        if RUBY_PLATFORM =~ /^.*darwin.*$/

            hapi_common_header = "/Library/Frameworks/Houdini.framework/Resources/toolkit/include/HAPI/HAPI_Common.h"

            if File.exists? hapi_common_header
                Rake::Task["erlang:generate_enums_from"].invoke hapi_common_header
            end
        end
    end

    # This will generate all necessary files.
    desc "Generate all necessary stubs."
    task :generate => ["erlang:generate_enums", "erlang:generate_erl"] do

    end

    # This will clean all binary files.
    desc "Clean"
    task :clean do

        sh 'rebar clean'
    end

    # This will compile erlang related code.
    desc "Compile"
    task :compile do

        if not File.exists? './deps/xxhash'
            puts "Missing xxhash dependency, please run 'rake deps' first."
        else
            sh 'rebar compile'
        end
    end

    # This will execute erlang shell with paths already set.
    desc "Run Erlang Shell with paths"
    task :runsh do

        begin
            sh 'erl -pa ebin'
        rescue Exception => e
        end
    end

    # Run otool on a resulting library. Darwin only.
    if RUBY_PLATFORM =~ /^.*darwin.*$/

        desc "Otool"
        task :otool do

            sh 'otool -l priv/hapi_nif.so | grep -A 3 RPATH'
        end
    end

    # This will pull all the dependencies required by project.
    desc "Download project dependencies"
    task :deps do

        sh 'rebar get-deps'
    end

    # Run testing.
    desc "Run unit tests"
    task :test do

        sh 'rebar compile eunit'
    end
end
