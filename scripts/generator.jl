# This script can be used to generate the low-level Julia wrapper for COPT. It
# requires you to manually set the COPT version number and the path to the
# appropriate header file.

using Clang.Generators

const COPT_VERSION = "4.0.6"
const LIBCOPT_HEADERS = ["/opt/copt40/include/copt.h"]

const GEN_DIR = joinpath(dirname(@__DIR__), "src", "gen$(COPT_VERSION)")
if !isdir(GEN_DIR)
    mkdir(GEN_DIR)
end

args = get_default_args()
options = load_options(joinpath(@__DIR__, "generator.toml"))
options["general"]["output_file_path"] = joinpath(GEN_DIR, "libcopt.jl")
ctx = create_context(LIBCOPT_HEADERS, args, options)
build!(ctx)
