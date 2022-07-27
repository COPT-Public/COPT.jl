# This command line program converts a zip archive to a tar.gz archive. When
# generating the COPT package for Windows we observed that the program must be
# executed on a Windows sytem. Otherwise after unpacking the zip file, the dll
# does not have executable permission, which causes the Julia Artifact-based
# installation to fail (see #3).
#
# Usage:
#
# Make sure that the necessary Julia packages are installed (import Pkg;
# Pkg.add("ArgParse"); Pkg.add("GZip"); Pkg.add("Tar"); Pkg.add("Zipfile");)
#
# Then run from a command line
#
# $ julia generate_tar_package.jl --zip_package CardinalOptimizer-4.0.7-win64.zip
#

using ArgParse
using GZip, Tar, ZipFile

# https://discourse.julialang.org/t/how-to-extract-a-file-in-a-zip-archive-without-using-os-specific-tools/34585/5
function unzip(file, exdir = "")
    fileFullPath = isabspath(file) ? file : joinpath(pwd(), file)
    basePath = dirname(fileFullPath)
    outPath = (
        exdir == "" ? basePath :
        (isabspath(exdir) ? exdir : joinpath(pwd(), exdir))
    )
    isdir(outPath) ? "" : mkdir(outPath)
    zarchive = ZipFile.Reader(fileFullPath)
    for f in zarchive.files
        fullFilePath = joinpath(outPath, f.name)
        if (endswith(f.name, "/") || endswith(f.name, "\\"))
            mkdir(fullFilePath)
        else
            write(fullFilePath, read(f))
        end
    end
    return close(zarchive)
end

function generate_tar_package(zip_package::String)
    if !endswith(zip_package, ".zip")
        error("Expected a filename ending with .zip, got $(zip_package)")
    end
    tar_package = replace(zip_package, r"\.zip$" => ".tar.gz")
    mktempdir() do dir
        unzip(zip_package, dir)
        GZip.open(tar_package, "w") do io
            return Tar.create(dir, io)
        end
    end
    return tar_package
end

function parse_command_line()
    s = ArgParseSettings()
    @add_arg_table! s begin
        "--zip_package"
        help = "The zip version of the COPT package."
        arg_type = String
        required = true
    end
    return ArgParse.parse_args(s)
end

function main()
    parsed_args = parse_command_line()
    zip_package = parsed_args["zip_package"]
    return generate_tar_package(zip_package)
end

main()
