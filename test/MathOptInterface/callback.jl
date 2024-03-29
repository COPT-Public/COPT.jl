# Copyright (c) 2023: COPT-Public
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestCallback

using COPT
using MathOptInterface
using Random
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

function callback_simple_model()
    env = COPT.Env()
    model = COPT.Optimizer(env)
    MOI.set(model, MOI.RawOptimizerAttribute("LogToConsole"), 0)
    MOI.Utilities.loadfromstring!(
        model,
        """
    variables: x, y
    maxobjective: y
    c1: x in Integer()
    c2: y in Integer()
    c3: x in Interval(0.0, 2.5)
    c4: y in Interval(0.0, 2.5)
""",
    )
    x = MOI.get(model, MOI.VariableIndex, "x")
    y = MOI.get(model, MOI.VariableIndex, "y")
    return model, x, y
end

function callback_knapsack_model()
    env = COPT.Env()
    model = COPT.Optimizer(env)
    MOI.set(model, MOI.RawOptimizerAttribute("LogToConsole"), 0)
    N = 30
    x = MOI.add_variables(model, N)
    MOI.add_constraints(model, x, MOI.ZeroOne())
    MOI.set.(model, MOI.VariablePrimalStart(), x, 0.0)
    Random.seed!(1)
    item_weights, item_values = rand(N), rand(N)
    MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(item_weights, x), 0.0),
        MOI.LessThan(10.0),
    )
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(item_values, x), 0.0),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    return model, x, item_weights
end

function test_lazy_constraint_callback()
    model, x, y = callback_simple_model()
    lazy_called = false
    MOI.set(
        model,
        MOI.LazyConstraintCallback(),
        cb_data -> begin
            lazy_called = true
            x_val = MOI.get(model, MOI.CallbackVariablePrimal(cb_data), x)
            y_val = MOI.get(model, MOI.CallbackVariablePrimal(cb_data), y)
            status = MOI.get(
                model,
                MOI.CallbackNodeStatus(cb_data),
            )::MOI.CallbackNodeStatusCode
            int_sol = round.(Int, [x_val, y_val])
            if status == MOI.CALLBACK_NODE_STATUS_INTEGER
                @test isapprox(int_sol, [x_val, y_val], atol = 1e-6)
            end
            @test MOI.supports(model, MOI.LazyConstraint(cb_data))
            if y_val - x_val > 1 + 1e-6
                @test MOI.submit(
                    model,
                    MOI.LazyConstraint(cb_data),
                    MOI.ScalarAffineFunction{Float64}(
                        MOI.ScalarAffineTerm.([-1.0, 1.0], [x, y]),
                        0.0,
                    ),
                    MOI.LessThan{Float64}(1.0),
                ) === nothing
            elseif y_val + x_val > 3 + 1e-6
                @test MOI.submit(
                    model,
                    MOI.LazyConstraint(cb_data),
                    MOI.ScalarAffineFunction{Float64}(
                        MOI.ScalarAffineTerm.([1.0, 1.0], [x, y]),
                        0.0,
                    ),
                    MOI.LessThan{Float64}(3.0),
                ) === nothing
            end
        end,
    )
    @test MOI.supports(model, MOI.LazyConstraintCallback())
    MOI.optimize!(model)
    @test lazy_called
    @test MOI.get(model, MOI.VariablePrimal(), x) == 1
    @test MOI.get(model, MOI.VariablePrimal(), y) == 2
end

function test_lazy_constraint_callback_fractional()
    model, x, item_weights = callback_knapsack_model()
    lazy_called_integer = false
    lazy_called_fractional = false
    MOI.set(
        model,
        MOI.LazyConstraintCallback(),
        cb_data -> begin
            status = MOI.get(
                model,
                MOI.CallbackNodeStatus(cb_data),
            )::MOI.CallbackNodeStatusCode
            if status == MOI.CALLBACK_NODE_STATUS_INTEGER
                lazy_called_integer = true
            elseif status == MOI.CALLBACK_NODE_STATUS_FRACTIONAL
                lazy_called_fractional = true
            end
        end,
    )
    @test MOI.supports(model, MOI.LazyConstraintCallback())
    MOI.optimize!(model)
    @test lazy_called_integer || lazy_called_fractional
end

function test_lazy_constraint_callback_OptimizeInProgress()
    model, x, y = callback_simple_model()
    MOI.set(
        model,
        MOI.LazyConstraintCallback(),
        cb_data -> begin
            @test_throws(
                MOI.OptimizeInProgress(MOI.VariablePrimal()),
                MOI.get(model, MOI.VariablePrimal(), x)
            )
            @test_throws(
                MOI.OptimizeInProgress(MOI.ObjectiveValue()),
                MOI.get(model, MOI.ObjectiveValue())
            )
            @test_throws(
                MOI.OptimizeInProgress(MOI.ObjectiveBound()),
                MOI.get(model, MOI.ObjectiveBound())
            )
        end,
    )
    return MOI.optimize!(model)
end

function test_lazy_constraint_callback_UserCut()
    model, x, y = callback_simple_model()
    cb = nothing
    MOI.set(
        model,
        MOI.LazyConstraintCallback(),
        cb_data -> begin
            cb = cb_data
            MOI.submit(
                model,
                MOI.UserCut(cb_data),
                MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
                MOI.LessThan(2.0),
            )
        end,
    )
    @test_throws(
        MOI.InvalidCallbackUsage(MOI.LazyConstraintCallback(), MOI.UserCut(cb)),
        MOI.optimize!(model)
    )
end

function test_lazy_constraint_callback_HeuristicSolution()
    model, x, y = callback_simple_model()
    cb = nothing
    MOI.set(
        model,
        MOI.LazyConstraintCallback(),
        cb_data -> begin
            cb = cb_data
            MOI.submit(model, MOI.HeuristicSolution(cb_data), [x], [2.0])
        end,
    )
    @test_throws(
        MOI.InvalidCallbackUsage(
            MOI.LazyConstraintCallback(),
            MOI.HeuristicSolution(cb),
        ),
        MOI.optimize!(model)
    )
end

# function test_user_cut_callback()
#     model, x, item_weights = callback_knapsack_model()
#     user_cut_submitted = false
#     MOI.set(
#         model,
#         MOI.UserCutCallback(),
#         cb_data -> begin
#             terms = MOI.ScalarAffineTerm{Float64}[]
#             accumulated = 0.0
#             for (i, xi) in enumerate(x)
#                 if MOI.get(model, MOI.CallbackVariablePrimal(cb_data), xi) > 0.0
#                     push!(terms, MOI.ScalarAffineTerm(1.0, xi))
#                     accumulated += item_weights[i]
#                 end
#             end
#             @test MOI.supports(model, MOI.UserCut(cb_data))
#             if accumulated > 10.0
#                 @test MOI.submit(
#                     model,
#                     MOI.UserCut(cb_data),
#                     MOI.ScalarAffineFunction{Float64}(terms, 0.0),
#                     MOI.LessThan{Float64}(length(terms) - 1),
#                 ) === nothing
#                 user_cut_submitted = true
#             end
#         end,
#     )
#     @test MOI.supports(model, MOI.UserCutCallback())
#     MOI.optimize!(model)
#     @test user_cut_submitted
# end

# function test_user_cut_callback_LazyConstraint()
#     model, x, item_weights = callback_knapsack_model()
#     cb = nothing
#     MOI.set(
#         model,
#         MOI.UserCutCallback(),
#         cb_data -> begin
#             cb = cb_data
#             MOI.submit(
#                 model,
#                 MOI.LazyConstraint(cb_data),
#                 MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(1.0, x), 0.0),
#                 MOI.LessThan(5.0),
#             )
#         end,
#     )
#     @test_throws(
#         MOI.InvalidCallbackUsage(MOI.UserCutCallback(), MOI.LazyConstraint(cb)),
#         MOI.optimize!(model)
#     )
# end

# function test_user_cut_callback_HeuristicSolution()
#     model, x, item_weights = callback_knapsack_model()
#     cb = nothing
#     MOI.set(
#         model,
#         MOI.UserCutCallback(),
#         cb_data -> begin
#             cb = cb_data
#             MOI.submit(model, MOI.HeuristicSolution(cb_data), [x[1]], [0.0])
#         end,
#     )
#     @test_throws(
#         MOI.InvalidCallbackUsage(
#             MOI.UserCutCallback(),
#             MOI.HeuristicSolution(cb),
#         ),
#         MOI.optimize!(model)
#     )
# end

function test_heuristic_callback()
    model, x, item_weights = callback_knapsack_model()
    MOI.set(
        model,
        MOI.HeuristicCallback(),
        cb_data -> begin
            x_vals = MOI.get.(model, MOI.CallbackVariablePrimal(cb_data), x)
            @test MOI.supports(model, MOI.HeuristicSolution(cb_data))
            status = MOI.get(
                model,
                MOI.CallbackNodeStatus(cb_data),
            )::MOI.CallbackNodeStatusCode
            int_sol = round.(Int, x_vals)
            if status == MOI.CALLBACK_NODE_STATUS_INTEGER
                @test isapprox(int_sol, x_vals, atol = 1e-6)
            end
            MOI.submit(
                model,
                MOI.HeuristicSolution(cb_data),
                x,
                floor.(x_vals),
            )
            MOI.submit(model, MOI.HeuristicSolution(cb_data), x, ceil.(x_vals))
        end,
    )
    @test MOI.supports(model, MOI.HeuristicCallback())
    return MOI.optimize!(model)
end

function test_heuristic_callback_LazyConstraint()
    model, x, item_weights = callback_knapsack_model()
    cb = nothing
    MOI.set(
        model,
        MOI.HeuristicCallback(),
        cb_data -> begin
            cb = cb_data
            MOI.submit(
                model,
                MOI.LazyConstraint(cb_data),
                MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(1.0, x), 0.0),
                MOI.LessThan(5.0),
            )
        end,
    )
    @test_throws(
        MOI.InvalidCallbackUsage(
            MOI.HeuristicCallback(),
            MOI.LazyConstraint(cb),
        ),
        MOI.optimize!(model)
    )
end

function test_heuristic_callback_UserCut()
    model, x, item_weights = callback_knapsack_model()
    cb = nothing
    MOI.set(
        model,
        MOI.HeuristicCallback(),
        cb_data -> begin
            cb = cb_data
            MOI.submit(
                model,
                MOI.UserCut(cb_data),
                MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(1.0, x), 0.0),
                MOI.LessThan(5.0),
            )
        end,
    )
    @test_throws(
        MOI.InvalidCallbackUsage(MOI.HeuristicCallback(), MOI.UserCut(cb)),
        MOI.optimize!(model)
    )
end

function test_CallbackFunction_callback_OptimizeInProgress()
    model, x, y = callback_simple_model()
    MOI.set(
        model,
        COPT.CallbackFunction(),
        (cb_data, cb_context) -> begin
            @test_throws(
                MOI.OptimizeInProgress(MOI.VariablePrimal()),
                MOI.get(model, MOI.VariablePrimal(), x)
            )
            @test_throws(
                MOI.OptimizeInProgress(MOI.ObjectiveValue()),
                MOI.get(model, MOI.ObjectiveValue())
            )
            @test_throws(
                MOI.OptimizeInProgress(MOI.ObjectiveBound()),
                MOI.get(model, MOI.ObjectiveBound())
            )
        end,
    )
    @test MOI.supports(model, COPT.CallbackFunction())
    return MOI.optimize!(model)
end

function test_CallbackFunction_callback_LazyConstraint()
    model, x, y = callback_simple_model()
    cb_calls = Int32[]
    function callback_function(cb_data::COPT.CallbackData, cb_context::Cint)
        push!(cb_calls, cb_context)
        if cb_context == COPT.COPT_CBCONTEXT_MIPSOL
            COPT.load_callback_variable_primal(cb_data, cb_context)
            x_val = MOI.get(model, MOI.CallbackVariablePrimal(cb_data), x)
            y_val = MOI.get(model, MOI.CallbackVariablePrimal(cb_data), y)
            if y_val - x_val > 1 + 1e-6
                MOI.submit(
                    model,
                    MOI.LazyConstraint(cb_data),
                    MOI.ScalarAffineFunction{Float64}(
                        MOI.ScalarAffineTerm.([-1.0, 1.0], [x, y]),
                        0.0,
                    ),
                    MOI.LessThan{Float64}(1.0),
                )
            elseif y_val + x_val > 3 + 1e-6
                MOI.submit(
                    model,
                    MOI.LazyConstraint(cb_data),
                    MOI.ScalarAffineFunction{Float64}(
                        MOI.ScalarAffineTerm.([1.0, 1.0], [x, y]),
                        0.0,
                    ),
                    MOI.LessThan{Float64}(3.0),
                )
            end
        end
    end
    MOI.set(model, COPT.CallbackFunction(), callback_function)
    MOI.optimize!(model)
    @test MOI.get(model, MOI.VariablePrimal(), x) == 1
    @test MOI.get(model, MOI.VariablePrimal(), y) == 2
    @test length(cb_calls) > 0
    @test COPT.COPT_CBCONTEXT_MIPSOL in cb_calls
end

# function test_CallbackFunction_callback_UserCut()
#     model, x, item_weights = callback_knapsack_model()
#     user_cut_submitted = false
#     cb_calls = Int32[]
#     MOI.set(
#         model,
#         COPT.CallbackFunction(),
#         (cb_data, cb_context) -> begin
#             push!(cb_calls, cb_context)
#             if cb_context != COPT.COPT_CBCONTEXT_MIPRELAX
#                 return
#             end
#             COPT.load_callback_variable_primal(cb_data, cb_context)
#             terms = MOI.ScalarAffineTerm{Float64}[]
#             accumulated = 0.0
#             for (i, xi) in enumerate(x)
#                 if MOI.get(model, MOI.CallbackVariablePrimal(cb_data), xi) > 0.0
#                     push!(terms, MOI.ScalarAffineTerm(1.0, xi))
#                     accumulated += item_weights[i]
#                 end
#             end
#             if accumulated > 10.0
#                 MOI.submit(
#                     model,
#                     MOI.UserCut(cb_data),
#                     MOI.ScalarAffineFunction{Float64}(terms, 0.0),
#                     MOI.LessThan{Float64}(length(terms) - 1),
#                 )
#                 user_cut_submitted = true
#             end
#         end,
#     )
#     MOI.optimize!(model)
#     @test user_cut_submitted
#     @test COPT.COPT_CBCONTEXT_MIPRELAX in cb_calls
# end

function test_CallbackFunction_callback_HeuristicSolution()
    model, x, item_weights = callback_knapsack_model()
    cb_calls = Int32[]
    MOI.set(
        model,
        COPT.CallbackFunction(),
        (cb_data, cb_context) -> begin
            push!(cb_calls, cb_context)
            COPT.load_callback_variable_primal(cb_data, cb_context)
            x_vals =
                MOI.get.(model, MOI.CallbackVariablePrimal(cb_data), x)
            MOI.submit(
                model,
                MOI.HeuristicSolution(cb_data),
                x,
                floor.(x_vals),
            )
        end,
    )
    MOI.optimize!(model)
    @test COPT.COPT_CBCONTEXT_MIPSOL in cb_calls
end

function test_CallbackFunction_CallbackNodeStatus()
    model, x, item_weights = callback_knapsack_model()
    unknown_reached = false
    MOI.set(
        model,
        COPT.CallbackFunction(),
        (cb_data, cb_context) -> begin
            if MOI.get(model, MOI.CallbackNodeStatus(cb_data)) ==
               MOI.CALLBACK_NODE_STATUS_UNKNOWN
                unknown_reached = true
            end
        end,
    )
    MOI.optimize!(model)
    @test !unknown_reached
end

function test_CallbackFunction_broadcast()
    model, x, _ = callback_knapsack_model()
    f(cb_data, x) = MOI.get(model, MOI.CallbackVariablePrimal(cb_data), x)
    solutions = Vector{Float64}[]
    MOI.set(
        model,
        COPT.CallbackFunction(),
        (cb_data, cb_context) -> begin
            if cb_context == COPT.COPT_CBCONTEXT_MIPSOL
                COPT.load_callback_variable_primal(cb_data, cb_context)
                push!(solutions, f(cb_data, x))
            end
        end,
    )
    MOI.optimize!(model)
    @test length(solutions) > 0
    @test length(solutions[1]) == length(x)
end

end  # module TestCallback

TestCallback.runtests()
