# This script can be used to generate the low-level Julia wrapper for COPT. It
# requires you to manually set the COPT version number and the path to the
# appropriate header file.

using Clang.Generators

const COPT_VERSION = "6.0.1"
const COPT_HEADERS = ["/opt/copt60/include/copt.h"]

const GEN_DIR = joinpath(dirname(@__DIR__), "src", "gen$(COPT_VERSION)")
if !isdir(GEN_DIR)
    mkdir(GEN_DIR)
end

args = get_default_args()
options = load_options(joinpath(@__DIR__, "generator.toml"))
options["general"]["output_file_path"] = joinpath(GEN_DIR, "libcopt.jl")
ctx = create_context(COPT_HEADERS, args, options)
build!(ctx)
