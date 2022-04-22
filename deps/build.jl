using Libdl

const _DEPS_FILE = joinpath(dirname(@__FILE__), "deps.jl")
if isfile(_DEPS_FILE)
    rm(_DEPS_FILE)
end

const _COPT_VERS = [ # From oldest to most recent.
    "40",
]
const _COPT_HOME_ENV = "COPT_HOME"

function write_depsfile(path)
    open(_DEPS_FILE, "w") do f
        println(f, "const libcopt = \"$(escape_string(path))\"")
    end
end

function library_name()
    prefix = Sys.iswindows() ? "" : "lib"
    return "$(prefix)copt.$(Libdl.dlext)"
end

function default_installation_path(copt_home_dir::AbstractString)
    if Sys.iswindows()
        return escape_string("C:\\Program Files\\$copt_home_dir")
    elseif Sys.isapple()
        return "/Applications/$copt_home_dir"
    else
        return "/opt/$copt_home_dir"
    end
end

function get_error_message_if_not_found()
    return """
    Unable to install COPT.jl.

    The versions of COPT supported by COPT.jl are:

    * 4.0

    You must download and install one of these versions separately.

    You should set the `COPT_HOME` environment variable to point to the install
    location then try again. For example (updating the path to the correct
    location):

    ```
    ENV["COPT_HOME"] = "$(default_installation_path("copt40"))"
    import Pkg
    Pkg.add("COPT")
    Pkg.build("COPT")
    ```

    See the COPT.jl README at https://github.com/COPT-Public/COPT.jl for further
    instructions.
    """
end

function check_copt_in_libnames(libnames)
    for l in libnames
        d = Libdl.dlopen_e(l)
        if d == C_NULL
            continue
        end
        return l
    end
    return nothing
end

function check_copt_in_environment_variables()
    # Find COPT in the COPT environment variables.
    libnames = String[]
    if haskey(ENV, _COPT_HOME_ENV)
        for path in split(ENV[_COPT_HOME_ENV], ';')
            if isdir(path)
                guessed_file = joinpath(path, "lib", library_name())
                if isfile(guessed_file)
                    push!(libnames, guessed_file)
                end
            end
        end
    end
    return libnames
end

function check_copt_in_default_paths()
    # Find COPT in the default installation locations, based on the platform.
    copt_home_dirs = String[]
    for v in reverse(_COPT_VERS)
        push!(copt_home_dirs, "copt$v")
    end
    push!(copt_home_dirs, "COPT")

    libnames = String[]
    for copt_home_dir in copt_home_dirs
        path = default_installation_path(copt_home_dir)
        if isdir(path)
            guessed_file = joinpath(path, "lib", library_name())
            if isfile(guessed_file)
                push!(libnames, guessed_file)
            end
        end
    end
    return libnames
end

function try_local_installation()
    # Iterate through a series of places where COPT could be found: either from
    # an environment variable or in a default install location, in that order.
    for libnames in [check_copt_in_environment_variables(), check_copt_in_default_paths()]
        found_copt_lib = check_copt_in_libnames(libnames)
        if found_copt_lib !== nothing
            write_depsfile(Libdl.dlpath(found_copt_lib))
            @info("Using COPT found in location `$(found_copt_lib)`")
            return
        end
    end

    error(get_error_message_if_not_found())
end

function try_ci_installation()
    error("CI installation is not supported")
    # CPLEX_VERSION = ENV["CPLEX_VERSION"]
    # url = ENV["SECRET_CPLEX_URL_" * CPLEX_VERSION]
    # local_filename = joinpath(@__DIR__, "libcplex.so")
    # download(url, local_filename)
    # write_depsfile(local_filename)
end

if get(ENV, "JULIA_REGISTRYCI_AUTOMERGE", "false") == "true"
    # We need to be able to install and load this package without error for
    # Julia's registry AutoMerge to work. Just write a fake libcopt path.
    write_depsfile("julia_registryci_automerge")
elseif get(ENV, "SECRET_CPLEX_URL_12100", "") != ""
    try_ci_installation()
else
    try_local_installation()
end
