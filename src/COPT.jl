# Copyright (c) 2013: Joey Huchette and contributors
# Copyright (c) 2022: COPT-Public
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module COPT

import Libdl
import Pkg

# deps.jl file is always built via `Pkg.build`, even if we didn't find a local
# install and we want to use the artifact instead. This is so COPT.jl will be
# recompiled if we update the file.
include(joinpath(dirname(@__DIR__), "deps", "deps.jl"))
if isdefined(@__MODULE__, :libcopt)
    # deps.jl must define a local installation.
elseif Sys.islinux() || Sys.isapple()
    # No local installation defined in deps.jl. Use the artifact instead.
    coptdir = "copt50"
    libdir = Sys.iswindows() ? "bin" : "lib"
    prefix = Sys.iswindows() ? "" : "lib"
    libname = prefix * "copt." * Libdl.dlext
    const libcopt =
        joinpath(Pkg.Artifacts.artifact"copt", coptdir, libdir, libname)
else
    error("""
        COPT not properly installed. Please run Pkg.build(\"COPT\"). For
        more information go to https://github.com/COPT-Public/COPT.jl
    """)
end

function _get_banner()
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
        return unsafe_string(pointer(buffer))
    end
    return error("COPT error $ret: Unable to get the banner.")
end

function _get_version_number()
    banner = _get_banner()
    m = match(r"Cardinal Optimizer v([0-9]+)\.([0-9]+)\.([0-9]+)", banner)
    if length(m.captures) == 3
        major, minor, technical = parse.(Int, m.captures)
        return VersionNumber(major, minor, technical)
    end
    error("Failed to detect the version of the COPT library")
    return VersionNumber(0, 0, 0)
end

function _get_codegen_dir(version::VersionNumber)
    # src/genX.Y.Z contains the low-level Julia wrapper around the C API for
    # COPT version X.Y.Z. We may not provide a low-level wrapper for each patch
    # version. Instead we assume that the COPT library is compatible with the
    # wrapper of a previous patch version.
    for patch in reverse(0:version.patch)
        gen_version = VersionNumber(version.major, version.minor, patch)
        dirname = "gen$(gen_version)"
        if isdir(joinpath(@__DIR__, dirname))
            return dirname
        end
    end
    return "gen$(version)"
end

const _COPT_VERSION = _get_version_number()
const _GEN_DIR = _get_codegen_dir(_COPT_VERSION)

if isdir(joinpath(@__DIR__, _GEN_DIR))
    include("$_GEN_DIR/libcopt.jl")
else
    error("""
You have installed version $_COPT_VERSION of COPT, which is not supported
by COPT.jl. We require at least COPT version 4.0.5.

If you have a newer version of COPT installed, changes may need to be made
to the Julia code. Please open an issue at
https://github.com/COPT-Public/COPT.jl.""")
end

include("MOI/MOI_wrapper.jl")
include("MOI/indicator_constraint.jl")

# COPT exports all `COPT_xxx` and `copt_xxx` symbols. If you don't want all of
# these symbols in your environment, then use `import COPT` instead of
# `using COPT`.

for sym in filter(
    s -> startswith("$s", "COPT_") || startswith("$s", "copt_"),
    names(@__MODULE__, all = true),
)
    @eval export $sym
end

function __init__()
    # Respect the -q and --banner flag
    silent = Base.JLOptions().banner == 0
    if !silent
        print(_get_banner())
    end
end

end
