# Copyright (c) 2013: Joey Huchette and contributors
# Copyright (c) 2021: Benoît Legat
# Copyright (c) 2022: COPT-Public
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConeOptimizer

using COPT
using LinearAlgebra
using MathOptInterface
using Test

const MOI = MathOptInterface

function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith("$(name)", "test_")
            @testset "$(name)" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
    return
end

function test_runtests()
    model = MOI.Utilities.CachingOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
        MOI.instantiate(COPT.ConeOptimizer; with_bridge_type = Float64),
    )
    @test model.optimizer.model.model_cache isa
          MOI.Utilities.UniversalFallback{COPT.OptimizerCache}
    # `Variable.ZerosBridge` makes dual needed by some tests fail.
    MOI.Bridges.remove_bridge(
        model.optimizer,
        MathOptInterface.Bridges.Variable.ZerosBridge{Float64},
    )
    MOI.set(model, MOI.Silent(), true)
    MOI.Test.runtests(
        model,
        MOI.Test.Config(
            rtol = 1e-3,
            atol = 1e-3,
            exclude = Any[
                MOI.ConstraintBasisStatus,
                MOI.VariableBasisStatus,
                MOI.ObjectiveBound,
            ],
        ),
        exclude = String[
            # exclude tests trigger failures with COPT 6.5.x (until 6.5.3):
            "test_conic_HermitianPositiveSemidefiniteConeTriangle_2",
            "test_conic_PositiveSemidefiniteConeSquare_VectorAffineFunction_2",
            "test_conic_PositiveSemidefiniteConeSquare_VectorOfVariables_2",
            "test_conic_PositiveSemidefiniteConeTriangle_VectorAffineFunction_2",
            "test_conic_PositiveSemidefiniteConeTriangle_VectorOfVariables_2",
            "test_conic_RootDetConeSquare",
            "test_conic_RootDetConeTriangle",
            "test_conic_RootDetConeTriangle_VectorAffineFunction",
            "test_conic_RootDetConeTriangle_VectorOfVariables",
        ],
    )
    return
end

end  # module

TestConeOptimizer.runtests()
