module Generator

import Clang.Generators as ClangGen
import Downloads
import Inflate
import Tar

"""
    generate_wrapper(copt_version::VersionNumber, header_file::AbstractString="")

Generates the low-level Julia wrapper for the given COPT version in directory
src/genX.Y.Z. `header_file` is the path to the C header file `copt.h` from which
the wrapper is built. If empty, then the COPT package is downloaded and
extracted to temporary locations.
"""
function generate_wrapper(
    copt_version::VersionNumber,
    header_file::AbstractString = "",
)
    # Create output directory.
    gen_dir = joinpath(dirname(@__DIR__), "src", "gen$(copt_version)")
    if !isdir(gen_dir)
        mkdir(gen_dir)
    end
    # If no header file is given, then download and extract the COPT package.
    if isempty(header_file)
        url = "https://pub.shanshu.ai/download/copt/$(copt_version)/linux64/CardinalOptimizer-$(copt_version)-lnx64.tar.gz"
        println("Downloading ", url)
        download_path = Downloads.download(url)
        extract_path =
            Tar.extract(IOBuffer(Inflate.inflate_gzip(download_path)))
        contents = readdir(extract_path)
        @assert length(contents) == 1
        copt_dir = contents[1]
        header_file = joinpath(extract_path, copt_dir, "include", "copt.h")
    end
    # Run Clang generator.
    args = ClangGen.get_default_args()
    options = ClangGen.load_options(joinpath(@__DIR__, "generator.toml"))
    options["general"]["output_file_path"] = joinpath(gen_dir, "libcopt.jl")
    ctx = ClangGen.create_context(header_file, args, options)
    ClangGen.build!(ctx)
    return nothing
end

end # module
