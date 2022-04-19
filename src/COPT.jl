module COPT

include(joinpath(dirname(@__DIR__), "deps", "deps.jl"))

function _get_version_number()
    buffer_size = 1024
    buffer = zeros(Cchar, buffer_size)
    ret = ccall(
        (:COPT_GetBanner, libcopt),
        Cint,
        (Ptr{Cchar}, Cint),
        buffer,
        buffer_size,
    )
    if ret == 0
        banner = unsafe_string(pointer(buffer))
        m = match(r"Cardinal Optimizer v([0-9]+)\.([0-9]+)\.([0-9]+)", banner)
        if length(m.captures) == 3
            major, minor, technical = parse.(Int, m.captures)
            return VersionNumber(major, minor, technical)
        end
    end
    error("Failed to detect the version of the COPT library")
    return VersionNumber(0, 0, 0)
end

const _COPT_VERSION = _get_version_number()
const _GEN_DIR = "gen$_COPT_VERSION"

if isdir(joinpath(@__DIR__, _GEN_DIR))
    include("$_GEN_DIR/libcopt.jl")
else
    error("""
You have installed version $_COPT_VERSION of COPT, which is not supported
by COPT.jl. We require COPT version 4.0.5.

If you have a newer version of COPT installed, changes may need to be made
to the Julia code. Please open an issue at
https://github.com/COPT-Public/COPT.jl.""")
end

include("MOI/MOI_wrapper.jl")

# COPT exports all `COPT_xxx` and `copt_xxx` symbols. If you don't want all of
# these symbols in your environment, then use `import COPT` instead of
# `using COPT`.

for sym in filter(
    s -> startswith("$s", "COPT_") || startswith("$s", "copt_"),
    names(@__MODULE__, all = true),
)
    @eval export $sym
end

end
