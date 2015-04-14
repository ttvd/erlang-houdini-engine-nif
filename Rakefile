require 'rake'

# Helper function to produce underscore version of a string.
def create_underscore(string)
    string.scan(/[A-Z][a-z]*/).join("_").downcase
end

# Helper function to produce camelcase version of underscore string.
def create_camelcase(string)
    string.split("_").each { |s| s.capitalize! }.join("")
end

# Helper function to create hapi.erl from template.
def fill_template(hapi_header)

    # Read template file.
    template_file = File.read "./src/hapi.erl.template"

    # Read hapi header.
    hapi_file = File.read hapi_header

    # Create buffer for exports.
    buffer_exports = []

    # Create buffer for functions.
    buffer_functions = []

    # Extract all hapi entries.
    hapi_entries = hapi_file.scan(/HAPI_DECL\s+HAPI_([^(]*)\(([^)]*)\);/i)
    hapi_entries.each do |entry|

        hapi_entry_name = "#{create_underscore entry[0]}"
        hapi_entry_param_string = entry[1].gsub(/(\S)[^\S\n]*\n[^\S\n]*(\S)/, '\1 \2').gsub("*", "").strip

        hapi_entry_params = hapi_entry_param_string.split(",")
        hapi_entry_params.map! { |parm| "_#{create_camelcase(parm.split(" ").last)}" }

        # Add function name and arity to export table.
        buffer_exports << "    #{hapi_entry_name}/#{hapi_entry_params.length}"

        # Create function entry.
        buffer_functions << "#{hapi_entry_name}(#{hapi_entry_params.join(", ")})"
    end

    # Replace export template entry.
    template_file.gsub!("%{HAPI_EXPORT_FUNCTIONS}%", buffer_exports.join(",#{$/}"))

    # Replace function template entry.
    template_file.gsub!("%{HAPI_IMPL_FUNCTIONS}%", buffer_functions.map {|t| "%#{$/}#{t} ->#{$/}    ?nif_stub."}.join("#{$/}#{$/}"))

    # Write out the file.
    File.open("./src/hapi.erl", 'w') { |file| file.write(template_file) }
end


# Set default task.
task :default => [:help]

#
desc "Help"
task :help do

    puts %x{rake --tasks}
end

# This will generate hapi.erl
desc "Generate hapi.erl from HAPI.h"
task :generate_hapi, [:hapi_header_path] do |t, args|
    hapi_path = args.values_at(:hapi_header_path)

    if not hapi_path.first.nil?
        hapi_path = hapi_path.first

        if File.file? hapi_path
            fill_template hapi_path
        else
            hapi_path = "#{hapi_path}/HAPI.h"

            if File.file? hapi_path
                fill_template hapi_path
            else
                puts "Could not locate HAPI.h"
            end
        end
    else
        puts "Please provide location of HAPI.h as parameter."
    end
end

# This will clean all binary files.
desc "Clean"
task :clean do

    sh 'rebar clean'
end

# This will compile erlang related code.
desc "Compile"
task :compile do

    sh 'rebar compile'
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
