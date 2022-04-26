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
    # Turn off presolve reductions so CPLEX will generate infeasibility
    # certificates.
    MOI.set(model, MOI.RawOptimizerAttribute("Presolve"), 0)
    MOI.Test.runtests(
        model,
        MOI.Test.Config(atol = 1e-3, rtol = 1e-3),
        exclude = String[
            # TODO(odow): new tests
            "test_unbounded_",
            "test_infeasible_",
            # COPT does not support nonconvex QCPs
            "test_quadratic_nonconvex_",
            "test_conic_SecondOrderCone_negative_post_bound_3",
            # COPT does not support QP/QCP/SOCP with discrete variables
            "test_quadratic_Integer_SecondOrderCone",
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

function test_ZeroOne_NONE()
    model = COPT.Optimizer()
    x = MOI.add_variable(model)
    c = MOI.add_constraint(model, x, MOI.ZeroOne())
    tmp = Ref{Cdouble}()
    COPT.COPT_GetColInfo(model.prob, "LB", 1, Cint[0], tmp)
    @test tmp[] == 0.0
    COPT.COPT_GetColInfo(model.prob, "UB", 1, Cint[0], tmp)
    @test tmp[] == 1.0
    MOI.delete(model, c)
    COPT.COPT_GetColInfo(model.prob, "LB", 1, Cint[0], tmp)
    @test tmp[] == -COPT.COPT_INFINITY
    COPT.COPT_GetColInfo(model.prob, "UB", 1, Cint[0], tmp)
    @test tmp[] == COPT.COPT_INFINITY
    return
end

function test_ZeroOne_LESS_THAN()
    model = COPT.Optimizer()
    x = MOI.add_variable(model)
    MOI.add_constraint(model, x, MOI.LessThan(2.0))
    c = MOI.add_constraint(model, x, MOI.ZeroOne())
    tmp = Ref{Cdouble}()
    COPT.COPT_GetColInfo(model.prob, "LB", 1, Cint[0], tmp)
    @test tmp[] == 0.0
    COPT.COPT_GetColInfo(model.prob, "UB", 1, Cint[0], tmp)
    @test tmp[] == 2.0
    MOI.delete(model, c)
    COPT.COPT_GetColInfo(model.prob, "LB", 1, Cint[0], tmp)
    @test tmp[] == -COPT.COPT_INFINITY
    COPT.COPT_GetColInfo(model.prob, "UB", 1, Cint[0], tmp)
    @test tmp[] == 2.0
    return
end

function test_ZeroOne_GREATER_THAN()
    model = COPT.Optimizer()
    x = MOI.add_variable(model)
    MOI.add_constraint(model, x, MOI.GreaterThan(-2.0))
    c = MOI.add_constraint(model, x, MOI.ZeroOne())
    tmp = Ref{Cdouble}()
    COPT.COPT_GetColInfo(model.prob, "LB", 1, Cint[0], tmp)
    @test tmp[] == -2.0
    COPT.COPT_GetColInfo(model.prob, "UB", 1, Cint[0], tmp)
    @test tmp[] == 1.0
    MOI.delete(model, c)
    COPT.COPT_GetColInfo(model.prob, "LB", 1, Cint[0], tmp)
    @test tmp[] == -2.0
    COPT.COPT_GetColInfo(model.prob, "UB", 1, Cint[0], tmp)
    @test tmp[] == COPT.COPT_INFINITY
    return
end

function test_ZeroOne_INTERVAL()
    model = COPT.Optimizer()
    x = MOI.add_variable(model)
    MOI.add_constraint(model, x, MOI.Interval(-2.0, 2.0))
    c = MOI.add_constraint(model, x, MOI.ZeroOne())
    tmp = Ref{Cdouble}()
    COPT.COPT_GetColInfo(model.prob, "LB", 1, Cint[0], tmp)
    @test tmp[] == -2.0
    COPT.COPT_GetColInfo(model.prob, "UB", 1, Cint[0], tmp)
    @test tmp[] == 2.0
    MOI.delete(model, c)
    COPT.COPT_GetColInfo(model.prob, "LB", 1, Cint[0], tmp)
    @test tmp[] == -2.0
    COPT.COPT_GetColInfo(model.prob, "UB", 1, Cint[0], tmp)
    @test tmp[] == 2.0
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
