using Test

@testset "MathOptInterface Tests" begin
    for file in readdir(joinpath(@__DIR__, "MathOptInterface"))
        include(joinpath("MathOptInterface", file))
    end
end
