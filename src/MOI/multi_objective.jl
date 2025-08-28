# Copyright (c) 2015: Joey Huchette and contributors
# Copyright (c) 2025: COPT-Public
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.


function _copt_get_int_attr_multi_obj(model::Optimizer, index::Int, name::String)
    p_value = Ref{Cint}()
    ret = COPT_MultiObjGetIntAttr(model.prob, Cint(index - 1), name, p_value)
    _check_ret(model, ret)
    return p_value[]
end

function _copt_get_dbl_attr_multi_obj(model::Optimizer, index::Int, name::String)
    p_value = Ref{Cdouble}()
    ret = COPT_MultiObjGetDblAttr(model.prob, Cint(index - 1), name, p_value)
    _check_ret(model, ret)
    return p_value[]
end

# ==============================================================================
#    objective function
# ==============================================================================
struct MultiObjectiveFunction <: MOI.AbstractModelAttribute
    index::Int
end

function MOI.set(
    model::Optimizer,
    attr::MultiObjectiveFunction,
    f::MOI.ScalarAffineFunction,
)
    num_vars = length(model.variable_info)
    obj = zeros(Float64, num_vars)
    for term in f.terms
        col = column(model, term.variable)
        obj[col] += term.coefficient
    end
    ret = COPT_MultiObjSetColObj(model.prob, Cint(attr.index - 1), num_vars, C_NULL, obj)
    _check_ret(model, ret)
    ret = COPT_MultiObjSetObjConst(model.prob, Cint(attr.index - 1), f.constant)
    _check_ret(model, ret)
    return
end

function _zero_multiobjective(model::Optimizer, attr::MultiObjectiveFunction)
    num_vars = length(model.variable_info)
    # COPT returns an error when calling COPT_SetColObj() with no columns.
    if num_vars > 0
        obj = zeros(Float64, num_vars)
        ret = COPT_MultiObjSetColObj(model.prob, Cint(attr.index - 1), num_vars, C_NULL, obj)
        _check_ret(model, ret)
    end
    ret = COPT_MultiObjSetObjConst(model.prob, Cint(attr.index - 1), f.constant)
    _check_ret(model, ret)
    return
end

function MOI.set(model::Optimizer, attr::MultiObjectiveFunction, ::MOI.ObjectiveSense, sense::MOI.OptimizationSense)
    ret = if sense == MOI.MIN_SENSE
        COPT_MultiObjSetObjSense(model.prob, Cint(attr.index - 1), COPT_MINIMIZE)
    elseif sense == MOI.MAX_SENSE
        COPT_MultiObjSetObjSense(model.prob, Cint(attr.index - 1), COPT_MAXIMIZE)
    else
        @assert sense == MOI.FEASIBILITY_SENSE
        _zero_multiobjective(model, attr)
        COPT_SetObjSense(model.prob, COPT_MINIMIZE)
    end
    _check_ret(model, ret)
    return
end

function MOI.get(model::Optimizer, attr::MultiObjectiveFunction, ::MOI.ObjectiveSense)
    objective_sense = _copt_get_int_attr_multi_obj(model, attr.index, "ObjSense")
    if objective_sense == COPT_MINIMIZE
        return MOI.MIN_SENSE
    else
        return MOI.MAX_SENSE
    end
end

function MOI.supports(
    model::Optimizer,
    ::MOI.ObjectiveFunction{MOI.VectorAffineFunction{Float64}},
)
    return true
end

function MOI.set(
    model::Optimizer,
    ::MOI.ObjectiveFunction{F},
    f::F,
) where {F<:MOI.VectorAffineFunction{Float64}}
    for (i, fi) in enumerate(MOI.Utilities.eachscalar(f))
        MOI.set(model, MultiObjectiveFunction(i), fi)
    end
    model.objective_type = _VECTOR_AFFINE
    return
end

function _get_multiobj_linear_part(model::Optimizer, index::Int)
    num_col = length(model.variable_info)
    values = Array{Cdouble}(undef, num_col)
    ret = COPT_MultiObjGetColObj(model.prob, Cint(index - 1), num_col, C_NULL, values)
    _check_ret(model, ret)
    return values
end

function MOI.get(
    model::Optimizer,
    ::MOI.ObjectiveFunction{MOI.VectorAffineFunction{Float64}},
)
    F = MOI.ScalarAffineFunction{Float64}
    f = F[]
    for i in 1:MOI.get(model, NumberOfObjectives())
        coefficients = _get_multiobj_linear_part(model, i)
        terms = MOI.ScalarAffineTerm{Float64}[]
        for (index, info) in model.variable_info
            coefficient = coefficients[info.column]
            if !iszero(coefficient)
                push!(terms, MOI.ScalarAffineTerm(coefficient, index))
            end
        end
        constant = _copt_get_dbl_attr_multi_obj(model, i, "ObjConst")
        cur_obj = MOI.ScalarAffineFunction(terms, constant[])
        push!(f, cur_obj)
    end
    return MOI.Utilities.operate(vcat, Float64, f...)
end


# ==============================================================================
#    model-related parameters and attributes
# ==============================================================================
struct NumberOfObjectives <: MOI.AbstractModelAttribute end

function MOI.get(model::Optimizer, ::NumberOfObjectives)
    return _copt_get_int_attr(model, "MultiObjs")
end

struct MultiObjTimeLimit <: MOI.AbstractModelAttribute end

function MOI.set(model::Optimizer, ::MultiObjTimeLimit, value::Real)
    MOI.set(model, MOI.RawOptimizerAttribute("MultiObjTimeLimit"), value)
    return
end

function MOI.get(model::Optimizer, ::MultiObjTimeLimit)
    return MOI.get(model, MOI.RawOptimizerAttribute("MultiObjTimeLimit"))
end

struct MultiObjParamMode <: MOI.AbstractModelAttribute end

function MOI.set(model::Optimizer, ::MultiObjParamMode, value::Int)
    MOI.set(model, MOI.RawOptimizerAttribute("MultiObjParamMode"), value)
    return
end

function MOI.get(model::Optimizer, ::MultiObjParamMode)
    return MOI.get(model, MOI.RawOptimizerAttribute("MultiObjParamMode"))
end


# ==============================================================================
#    objective-related parameters
# ==============================================================================
struct MultiObjectiveParams <: MOI.AbstractModelAttribute
    index::Int
    name::String
end

function MOI.set(model::Optimizer, attr::MultiObjectiveParams, value)
    if (attr.name == COPT_MULTIOBJ_PRIORITY || attr.name == COPT_MULTIOBJ_WEIGHT || 
        attr.name == COPT_MULTIOBJ_ABSTOL || attr.name == COPT_MULTIOBJ_RELTOL)
        ret = COPT_MultiObjSetObjParam(model.prob, Cint(attr.index - 1), attr.name, value)
        _check_ret(model, ret)
        return
    end

    param_type = _search_param_attr(model, attr.name)
    if param_type == 0
        ret = COPT_MultiObjSetDblParam(model.prob,  Cint(attr.index - 1), attr.name, value)
        _check_ret(model, ret)
    elseif param_type == 1
        ret = COPT_MultiObjSetIntParam(model.prob, Cint(attr.index - 1), attr.name, value)
        _check_ret(model, ret)
    else
        throw(MOI.UnsupportedAttribute(attr.name))
    end
    return
end

function MOI.get(model::Optimizer, attr::MultiObjectiveParams)
    if (attr.name == COPT_MULTIOBJ_PRIORITY || attr.name == COPT_MULTIOBJ_WEIGHT || 
        attr.name == COPT_MULTIOBJ_ABSTOL || attr.name == COPT_MULTIOBJ_RELTOL)
        p_value = Ref{Cdouble}()
        ret = COPT_MultiObjGetObjParam(model.prob, Cint(attr.index - 1), attr.name, p_value)
        _check_ret(model, ret)
        return p_value[]
    end

    param_type = _search_param_attr(model, attr.name)
    if param_type == 0
        p_value = Ref{Cdouble}()
        ret = COPT_MultiObjGetDblParam(model.prob, Cint(attr.index - 1), attr.name, p_value)
        _check_ret(model, ret)
        return p_value[]
    elseif param_type == 1
        p_value = Ref{Cint}()
        ret = COPT_MultiObjGetIntParam(model.prob, Cint(attr.index - 1), attr.name, p_value)
        _check_ret(model, ret)
        return p_value[]
    else
        throw(MOI.UnsupportedAttribute(attr.name))
    end
end

struct MultiObjectivePriority <: MOI.AbstractModelAttribute
    index::Int
end

function MOI.set(model::Optimizer, attr::MultiObjectivePriority, value::Real)
    MOI.set(model, MultiObjectiveParams(attr.index, "MultiObjPriority"), value)
    return
end

function MOI.get(model::Optimizer, attr::MultiObjectivePriority)
    return MOI.get(model, MultiObjectiveParams(attr.index, "MultiObjPriority"))
end

struct MultiObjectiveWeight <: MOI.AbstractModelAttribute
    index::Int
end

function MOI.set(model::Optimizer, attr::MultiObjectiveWeight, weight::Real)
    MOI.set(model, MultiObjectiveParams(attr.index, "MultiObjWeight"), weight)
    return
end

function MOI.get(model::Optimizer, attr::MultiObjectiveWeight)
    return MOI.get(model, MultiObjectiveParams(attr.index, "MultiObjWeight"))
end

struct MultiObjectiveAbsTol <: MOI.AbstractModelAttribute
    index::Int
end

function MOI.set(model::Optimizer, attr::MultiObjectiveAbsTol, weight::Real)
    MOI.set(model, MultiObjectiveParams(attr.index, "MultiObjAbsTol"), weight)
    return
end

function MOI.get(model::Optimizer, attr::MultiObjectiveAbsTol)
    return MOI.get(model, MultiObjectiveParams(attr.index, "MultiObjAbsTol"))
end

struct MultiObjectiveRelTol <: MOI.AbstractModelAttribute
    index::Int
end

function MOI.set(model::Optimizer, attr::MultiObjectiveRelTol, weight::Real)
    MOI.set(model, MultiObjectiveParams(attr.index, "MultiObjRelTol"), weight)
    return
end

function MOI.get(model::Optimizer, attr::MultiObjectiveRelTol)
    return MOI.get(model, MultiObjectiveParams(attr.index, "MultiObjRelTol"))
end


# ==============================================================================
#    objective-related attributes
# ==============================================================================

struct MultiObjectiveValue <: MOI.AbstractModelAttribute
    index::Int
end

function MOI.get(model::Optimizer, attr::MultiObjectiveValue)
    return _copt_get_dbl_attr_multi_obj(model, attr.index, model.solved_as_mip ? "BestObj" : "LpObjval")
end