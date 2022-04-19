# TODO(lschork2):
#
# Automatically detect the location of the COPT library. See
# https://github.com/jump-dev/CPLEX.jl/blob/master/deps/build.jl
# https://github.com/jump-dev/Gurobi.jl/blob/master/deps/build.jl

const _DEPS_FILE = joinpath(dirname(@__FILE__), "deps.jl")
if isfile(_DEPS_FILE)
    rm(_DEPS_FILE)
end

function write_depsfile(path)
    open(_DEPS_FILE, "w") do f
        println(f, "const libcopt = \"$(escape_string(path))\"")
    end
end

write_depsfile("/usr/local/copt/lib/libcopt.so")
