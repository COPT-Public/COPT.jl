# Copyright (c) 2013: Joey Huchette and contributors
# Copyright (c) 2022: COPT-Public
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestMOIwrapper

using COPT
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
    model = MOI.Bridges.full_bridge_optimizer(COPT.Optimizer(), Float64)
    MOI.set(model, MOI.Silent(), true)
    MOI.set(model, MOI.RawOptimizerAttribute("Presolve"), 0)
    MOI.Test.runtests(
        model,
        MOI.Test.Config(atol = 1e-3, rtol = 1e-3),
        exclude = String[
            # exclude tests that trigger bugs in COPT 6.0.1
            "test_constraint_ZeroOne_bounds_3",
            "test_linear_Indicator_ON_ONE",
            "test_linear_Indicator_ON_ZERO",
            "test_linear_Indicator_constant_term",
            "test_linear_Indicator_integration",
            # exclude tests that trigger bugs in COPT 6.5.x
            "test_linear_Semiinteger_integration",
            "test_linear_Semicontinuous_integration",
            # IIS is not available for feasible model
            "test_solve_conflict_feasible",
            # TODO(odow): new tests
            "test_unbounded_",
            "test_infeasible_",
            # COPT does not support nonconvex QCPs
            "test_quadratic_nonconvex_",
            "test_conic_SecondOrderCone_negative_post_bound_3",
            # COPT does not support quadratic constraints with empty Q matrix
            "test_basic_VectorQuadraticFunction_GeometricMeanCone",
            # COPT does not provide dual solution for quadratic constraints
            "test_conic_GeometricMeanCone_VectorAffineFunction",
            "test_conic_GeometricMeanCone_VectorAffineFunction_2",
            "test_conic_GeometricMeanCone_VectorOfVariables",
            "test_conic_GeometricMeanCone_VectorOfVariables_2",
            "test_conic_RotatedSecondOrderCone_INFEASIBLE_2",
            "test_conic_RotatedSecondOrderCone_VectorAffineFunction",
            "test_conic_RotatedSecondOrderCone_VectorOfVariables",
            "test_conic_RotatedSecondOrderCone_out_of_order",
            "test_conic_SecondOrderCone_Nonnegatives",
            "test_conic_SecondOrderCone_Nonpositives",
            "test_conic_SecondOrderCone_VectorAffineFunction",
            "test_conic_SecondOrderCone_VectorOfVariables",
            "test_conic_SecondOrderCone_out_of_order",
            "test_constraint_PrimalStart_DualStart_SecondOrderCone",
            "test_quadratic_SecondOrderCone_basic",
            "test_quadratic_constraint_GreaterThan",
            "test_quadratic_constraint_LessThan",
            "test_quadratic_constraint_basic",
            "test_quadratic_constraint_integration",
            "test_quadratic_constraint_minimize",
            # New tests for function modifcation, https://github.com/jump-dev/MathOptInterface.jl/pull/2328
            "test_basic_VectorAffineFunction_Indicator_LessThan",
            "test_basic_VectorAffineFunction_Indicator_GreaterThan",
            # COPT does not support delete variables in multi-objective Optimization 
            "test_multiobjective_vector_affine_function_delete",
            "test_multiobjective_vector_quadratic_function_delete",
            "test_multiobjective_vector_of_variables_delete"
        ],
    )
    return
end

function test_user_provided_env()
    env = COPT.Env()
    model_1 = COPT.Optimizer(env)
    @test model_1.env === env
    model_2 = COPT.Optimizer(env)
    @test model_2.env === env
    # Check that finalizer doesn't touch env when manually provided.
    finalize(model_1)
    @test env.ptr != C_NULL
    return
end

function test_automatic_env()
    model_1 = COPT.Optimizer()
    model_2 = COPT.Optimizer()
    @test model_1.env.ptr !== model_2.env.ptr
    return
end

function test_user_provided_env_empty()
    env = COPT.Env()
    model = COPT.Optimizer(env)
    @test model.env === env
    @test env.ptr != C_NULL
    MOI.empty!(model)
    @test model.env === env
    @test env.ptr != C_NULL
    return
end

function test_automatic_env_empty()
    model = COPT.Optimizer()
    env = model.env
    MOI.empty!(model)
    @test model.env === env
    @test env.ptr != C_NULL
    return
end

function test_manual_env()
    env = COPT.Env()
    model = COPT.Optimizer(env)
    finalize(env)
    @test env.finalize_called
    finalize(model)
    @test env.ptr == C_NULL
    return
end

function test_fake_status()
    model = COPT.Optimizer()
    model.ret_optimize = COPT.COPT_RETCODE_MEMORY
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.MEMORY_LIMIT
    @test MOI.get(model, MOI.RawStatusString()) == "Memory allocation failure."
    return
end

function test_PassNames()
    model = COPT.Optimizer()
    @test model.pass_names == false
    MOI.set(model, COPT.PassNames(), true)
    @test model.pass_names == true
    return
end

end  # module TestMOIwrapper

TestMOIwrapper.runtests()
