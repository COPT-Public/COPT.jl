# Copyright (c) 2015: Joey Huchette and contributors
# Copyright (c) 2025: COPT-Public
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestMultiobjective

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

function test_multiobjective_func()
    env = COPT.Env()
    model = COPT.Optimizer(env)
    MOI.set(model, MOI.Silent(), true)

    if !(MOI.supports(model, MOI.ObjectiveFunction{MOI.VectorAffineFunction{Float64}}()))
        return
    end

    ## test initial value of "NumberOfObjectives"
    @test MOI.get(model, COPT.NumberOfObjectives()) == 0

    x = MOI.add_variable(model)
    y = MOI.add_variable(model)

    f = MOI.ScalarAffineFunction(
        [MOI.ScalarAffineTerm(1.0, x), MOI.ScalarAffineTerm(3.0, y)],
        0.0,
    )
    MOI.set(model, COPT.MultiObjectiveFunction(1), f)
    f_2 = MOI.ScalarAffineFunction(
        [MOI.ScalarAffineTerm(2.0, x), MOI.ScalarAffineTerm(-1.0, y)],
        0.0,
    )
    MOI.set(model, COPT.MultiObjectiveFunction(2), f_2)

    ## test value of "NumberOfObjectives" after adding objective value
    @test MOI.get(model, COPT.NumberOfObjectives()) == 2

    ## test COPT.MultiObjParamMode struct
    @test MOI.get(model, COPT.MultiObjParamMode()) == 0
    MOI.set(model, COPT.MultiObjParamMode(), 1)
    @test MOI.get(model, COPT.MultiObjParamMode()) == 1

    ## test COPT.MultiObjTimeLimit struct
    MOI.set(model, COPT.MultiObjTimeLimit(), 600.5)
    @test MOI.get(model, COPT.MultiObjTimeLimit()) == 600.5

    ## test COPT.MultiObjectiveAttribute struct
    MOI.set(model, COPT.MultiObjectiveParams(1, "RelGap"), 0.05)
    @test MOI.get(model, COPT.MultiObjectiveParams(1, "RelGap")) == 0.05

    ## test COPT.MultiObjectiveWeight & COPT.MultiObjectivePriority & COPT.MultiObjectiveAbsTol & COPT.MultiObjectiveRelTol
    @test MOI.get(model, COPT.MultiObjectiveWeight(1)) == 1.0
    @test MOI.get(model, COPT.MultiObjectiveWeight(2)) == 1.0
    @test MOI.get(model, COPT.MultiObjectivePriority(1)) == 0
    @test MOI.get(model, COPT.MultiObjectivePriority(2)) == 0
    @test MOI.get(model, COPT.MultiObjectiveAbsTol(1)) == 0.000001
    @test MOI.get(model, COPT.MultiObjectiveAbsTol(2)) == 0.000001
    @test MOI.get(model, COPT.MultiObjectiveRelTol(1)) == 0
    @test MOI.get(model, COPT.MultiObjectiveRelTol(2)) == 0
    MOI.set(model, COPT.MultiObjectiveWeight(1), 1.5)
    @test MOI.get(model, COPT.MultiObjectiveWeight(1)) == 1.5
    MOI.set(model, COPT.MultiObjectivePriority(2), 2)
    @test MOI.get(model, COPT.MultiObjectivePriority(2)) == 2
    MOI.set(model, COPT.MultiObjectiveAbsTol(1), 0.001)
    @test MOI.get(model, COPT.MultiObjectiveAbsTol(1)) == 0.001
    MOI.set(model, COPT.MultiObjectiveRelTol(2), 0.1)
    @test MOI.get(model, COPT.MultiObjectiveRelTol(2)) == 0.1

    ## test sense of multiobjective
    @test MOI.get(model, COPT.MultiObjectiveFunction(2), MOI.ObjectiveSense()) == MOI.MIN_SENSE
    MOI.set(model, COPT.MultiObjectiveFunction(2), MOI.ObjectiveSense(), MOI.MAX_SENSE)
    @test MOI.get(model, COPT.MultiObjectiveFunction(2), MOI.ObjectiveSense()) == MOI.MAX_SENSE
end


function test_example_biobjective_knapsack()
    p1 = [77.0, 94, 71, 63, 96, 82, 85, 75, 72, 91, 99, 63, 84, 87, 79, 94, 90]
    p2 = [65.0, 90, 90, 77, 95, 84, 70, 94, 66, 92, 74, 97, 60, 60, 65, 97, 93]
    w = [80.0, 87, 68, 72, 66, 77, 99, 85, 70, 93, 98, 72, 100, 89, 67, 86, 91]

    env = COPT.Env()
    model = COPT.Optimizer(env)
    MOI.set(model, MOI.Silent(), true)

    if !(MOI.supports(model, MOI.ObjectiveFunction{MOI.VectorAffineFunction{Float64}}()))
        return
    end

    x = MOI.add_variables(model, length(w))
    MOI.add_constraint.(model, x, MOI.ZeroOne())
    MOI.add_constraint(model, w' * x, MOI.LessThan(900.0))
    obj_f = MOI.Utilities.operate(vcat, Float64, p1' * x, p2' * x)
    MOI.set(model, MOI.ObjectiveFunction{typeof(obj_f)}(), obj_f)
    
    for i in 1:MOI.get(model, COPT.NumberOfObjectives())
        MOI.set(model, COPT.MultiObjectiveFunction(i), MOI.ObjectiveSense(), MOI.MAX_SENSE)
    end

    MOI.optimize!(model)
    
    @test MOI.get(model, COPT.MultiObjectiveValue(1)) == 934.0
    @test MOI.get(model, COPT.MultiObjectiveValue(2)) == 971.0

    target_variables_values = [2, 3, 5, 6, 8, 10, 11, 12, 15, 16, 17]
    X = findall(elt -> elt > 0.9, MOI.get.(model, MOI.VariablePrimal(), x))
    @test X == target_variables_values

    return
end

end

TestMultiobjective.runtests()
