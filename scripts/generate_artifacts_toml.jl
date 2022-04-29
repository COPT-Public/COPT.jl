using Tar, Inflate, SHA

# COPT version that should be downloaded by the automatic installation.
const version = "4.0.7"

# Directory containing the COPT packages for all platforms. Needed only for
# computing their hashes.
const pkgdir = joinpath(ENV["HOME"], "Downloads")

const pkgname = Dict{Tuple,String}(
    ("aarch64", "linux") => "CardinalOptimizer-$(version)-aarch64_lnx.tar.gz",
    ("aarch64", "macos") => "CardinalOptimizer-$(version)-aarch64_mac.tar.gz",
    ("x86_64", "linux") => "CardinalOptimizer-$(version)-lnx64.tar.gz",
    ("x86_64", "macos") => "CardinalOptimizer-$(version)-osx64.tar.gz",
    ("x86_64", "windows") => "CardinalOptimizer-$(version)-win64.zip",
)

const urlbase = Dict{Tuple,String}(
    (
        "aarch64",
        "linux",
    ) => "https://pub.shanshu.ai/download/copt/$(version)/aarch64",
    (
        "aarch64",
        "macos",
    ) => "https://pub.shanshu.ai/download/copt/$(version)/aarch64",
    (
        "x86_64",
        "linux",
    ) => "https://pub.shanshu.ai/download/copt/$(version)/linux64",
    (
        "x86_64",
        "macos",
    ) => "https://pub.shanshu.ai/download/copt/$(version)/osx64",
    (
        "x86_64",
        "windows",
    ) => "https://pub.shanshu.ai/download/copt/$(version)/win64",
)

function get_sha256(pkgname::AbstractString)
    filename = joinpath(pkgdir, pkgname)
    return bytes2hex(open(sha256, filename))
end

function get_tree_sha1(pkgname::AbstractString)
    filename = joinpath(pkgdir, pkgname)
    return Tar.tree_hash(IOBuffer(inflate_gzip(filename)))
end

quote_string(s::AbstractString) = "\"$s\""

function write_artifacts_toml()
    f = open(joinpath(dirname(@__DIR__), "Artifacts.toml"), "w")
    # Automatic installation on Windows is not supported because Pkg only
    # supports downloading of tar files, not zip files.
    for arch in ["aarch64"; "x86_64"]
        for os in ["linux"; "macos"]
            pkg = pkgname[arch, os]
            url = urlbase[arch, os] * '/' * pkg
            println(f, "[[copt]]")
            println(f, "arch = ", quote_string(arch))
            println(f, "os = ", quote_string(os))
            println(f, "git-tree-sha1 = ", quote_string(get_tree_sha1(pkg)))
            println(f)
            println(f, "    [[copt.download]]")
            println(f, "    url = ", quote_string(url))
            println(f, "    sha256 = ", quote_string(get_sha256(pkg)))
            println(f)
        end
    end
    return close(f)
end

write_artifacts_toml()
