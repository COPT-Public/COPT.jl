# Copyright (c) 2013: Joey Huchette and contributors
# Copyright (c) 2021: Benoît Legat
# Copyright (c) 2022: COPT-Public
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConeOptimizer

using COPT
using JuMP
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
            rtol = 1e-4,
            atol = 1e-4,
            exclude = Any[
                MOI.ConstraintBasisStatus,
                MOI.VariableBasisStatus,
                MOI.ObjectiveBound,
            ],
        ),
        exclude = String[
        # Expected test failures:
        ],
    )
    return
end

function test_sdp_simple()
    model = JuMP.Model(COPT.ConeOptimizer)

    C = [1.0 -1.0; -1.0 2.0]

    @variable(model, X[1:2, 1:2], PSD)
    @variable(model, z[1:2] >= 0)
    @objective(model, Min, C ⋅ X)
    @constraint(model, c1, X[1, 1] - z[1] == 1)
    @constraint(model, c2, X[2, 2] - z[2] == 1)
    optimize!(model)

    solution_summary(model)

    @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMAL
    @test value.(X) ⋅ C ≈ dual(c1) + dual(c2)
end

end  # module

TestConeOptimizer.runtests()
