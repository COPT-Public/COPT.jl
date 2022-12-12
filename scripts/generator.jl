module Generator

import Clang.Generators as ClangGen
import Downloads
import Inflate
import SHA
import Tar

function get_pkg_url(
    version::VersionNumber,
    arch::AbstractString,
    os::AbstractString,
)
    dict = Dict{Tuple,Tuple}(
        ("aarch64", "linux") => (
            "CardinalOptimizer-$(version)-aarch64_lnx.tar.gz",
            "https://pub.shanshu.ai/download/copt/$(version)/aarch64",
        ),
        ("aarch64", "macos") => (
            "CardinalOptimizer-$(version)-aarch64_mac.tar.gz",
            "https://pub.shanshu.ai/download/copt/$(version)/aarch64",
        ),
        ("x86_64", "linux") => (
            "CardinalOptimizer-$(version)-lnx64.tar.gz",
            "https://pub.shanshu.ai/download/copt/$(version)/linux64",
        ),
        ("x86_64", "macos") => (
            "CardinalOptimizer-$(version)-osx64.tar.gz",
            "https://pub.shanshu.ai/download/copt/$(version)/osx64",
        ),
        ("x86_64", "windows") => (
            "CardinalOptimizer-$(version)-win64.tar.gz",
            "https://pub.shanshu.ai/download/copt/$(version)/win64",
        ),
    )
    if !haskey(dict, (arch, os))
        return nothing, nothing
    end
    pkgname, urlbase = dict[arch, os]
    url = urlbase * "/" * pkgname
    return pkgname, url
end

function get_sha256(pkgpath::AbstractString)
    return bytes2hex(open(SHA.sha256, pkgpath))
end

function get_tree_sha1(pkgpath::AbstractString)
    return Tar.tree_hash(IOBuffer(Inflate.inflate_gzip(pkgpath)))
end

quote_string(s::AbstractString) = "\"$s\""

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
        _, url = get_pkg_url(copt_version, "x86_64", "linux")
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

"""
    function generate_artifacts_toml(copt_version::VersionNumber, pkgdir::AbstractString = "")

Generates `Artifacts.toml` for the given COPT version. Requires the tar.gz
packages for all supported architectures and operating systems. Packages that
are not present in `pkgdir` are downloaded to `pkgdir`. If `pkgdir` is `""`,
then a temporary location is used.
"""
function generate_artifacts_toml(
    copt_version::VersionNumber,
    pkgdir::AbstractString = "",
)
    # Create directory for downloading the tar.gz packages.
    if isempty(pkgdir)
        pkgdir = mktempdir()
    elseif !isdir(pkgdir)
        mkdir(pkgdir)
    end
    f = open(joinpath(dirname(@__DIR__), "Artifacts.toml"), "w")
    for arch in ["aarch64"; "x86_64"]
        for os in ["linux"; "macos"; "windows"]
            pkgname, url = get_pkg_url(copt_version, arch, os)
            if pkgname == nothing
                continue # unsupported combination of architecture and OS
            end
            pkgpath = joinpath(pkgdir, pkgname)
            if !isfile(pkgpath)
                println("Downloading ", url)
                Downloads.download(url, pkgpath)
            end
            println(f, "[[copt]]")
            println(f, "arch = ", quote_string(arch))
            println(f, "os = ", quote_string(os))
            println(f, "git-tree-sha1 = ", quote_string(get_tree_sha1(pkgpath)))
            println(f, "lazy = true")
            println(f)
            println(f, "    [[copt.download]]")
            println(f, "    url = ", quote_string(url))
            println(f, "    sha256 = ", quote_string(get_sha256(pkgpath)))
            println(f)
        end
    end
    close(f)
    return nothing
end

end # module
