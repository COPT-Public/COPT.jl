# Copyright (c) 2013: Joey Huchette and contributors
# Copyright (c) 2021: Benoît Legat
# Copyright (c) 2022: COPT-Public
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

import MathOptInterface
using LinearAlgebra
using SparseArrays

const MOI = MathOptInterface
const CleverDicts = MOI.Utilities.CleverDicts

include("psd_cone_bridge.jl")

@enum(
    _BoundType,
    _NONE,
    _LESS_THAN,
    _GREATER_THAN,
    _LESS_AND_GREATER_THAN,
    _INTERVAL,
    _EQUAL_TO,
)

@enum(
    _ObjectiveType,
    _SINGLE_VARIABLE,
    _SCALAR_AFFINE,
    _SCALAR_QUADRATIC,
    _UNSET_OBJECTIVE,
)

const _SCALAR_SETS = Union{
    MOI.GreaterThan{Float64},
    MOI.LessThan{Float64},
    MOI.EqualTo{Float64},
    MOI.Interval{Float64},
}

#
# The COPT SDP interface specifies the primal/dual pair
#
# min c'x,       max b'y
# s.t. Ax = b,   c - A'y ∈ K
#       x ∈ K
#
# where K is a product of `MOI.Zeros`, `MOI.Nonnegatives`, `MOI.SecondOrderCone`
# and `ReorderedPSDCone`.

# This wrapper copies the MOI problem to the COPT dual so the natively supported
# sets are `VectorAffineFunction`-in-`S` where `S` is one of the sets just
# listed above.
#
# The wrapper is an adaption of the SeDuMi interface written by Beniot Legat and
# Miles Lubin.

MOI.Utilities.@product_of_sets(
    Cones,
    MOI.Zeros,
    MOI.Nonnegatives,
    MOI.SecondOrderCone,
    ReorderedPSDCone,
)

const OptimizerCache = MOI.Utilities.GenericModel{
    Float64,
    MOI.Utilities.ObjectiveContainer{Float64},
    MOI.Utilities.VariablesContainer{Float64},
    MOI.Utilities.MatrixOfConstraints{
        Float64,
        MOI.Utilities.MutableSparseMatrixCSC{
            Float64,
            Int,
            MOI.Utilities.OneBasedIndexing,
        },
        Vector{Float64},
        Cones{Float64},
    },
}

mutable struct _VariableInfo
    index::MOI.VariableIndex
    column::Int
    bound::_BoundType
    type::Cchar
    start::Union{Float64,Nothing}
    name::String
    # Storage for the lower bound if the variable is the `t` variable in a
    # second order cone.
    lower_bound_if_soc::Float64
    num_soc_constraints::Int
    function _VariableInfo(index::MOI.VariableIndex, column::Int)
        return new(index, column, _NONE, COPT_CONTINUOUS, nothing, "", NaN, 0)
    end
end

mutable struct _ConstraintInfo
    row::Int
    set::MOI.AbstractSet
    # Storage for constraint names. Where possible, these are also stored in the
    # COPT model.
    name::String
    _ConstraintInfo(row::Int, set::MOI.AbstractSet) = new(row, set, "")
end

mutable struct ConeSolution
    x::Vector{Float64}
    y::Vector{Float64}
    slack::Vector{Float64}
    objective_value::Float64
    dual_objective_value::Float64
    lpStatus::Cint
end

mutable struct Env
    ptr::Ptr{copt_env}
    # These fields keep track of how many models the `Env` is used for to help
    # with finalizing. We first finalize the attached models, then the
    # environment.
    finalize_called::Bool
    attached_models::Int

    function Env()
        p_ptr = Ref{Ptr{copt_env}}(C_NULL)
        ret = COPT_CreateEnv(p_ptr)
        if ret != COPT_RETCODE_OK
            error("COPT error $ret: Unable to create COPT environment.")
        end
        env = new(p_ptr[], false, 0)
        finalizer(env) do e
            e.finalize_called = true
            if e.attached_models == 0
                # Only finalize the model if there are no models using it.
                COPT_DeleteEnv(Ref(e.ptr))
                e.ptr = C_NULL
            end
        end
        return env
    end
end

function _get_error_string(env::Env, ret::Cint)
    buffer = zeros(Cchar, COPT_BUFFSIZE)
    if COPT_GetRetcodeMsg(ret, pointer(buffer), length(buffer)) ==
       COPT_RETCODE_OK
        return unsafe_string(pointer(buffer))
    end
    return "COPT error $(ret): Unknown error code."
end

function _check_ret(env::Env, ret::Cint)
    if ret == COPT_RETCODE_OK
        return
    end
    return error(_get_error_string(env, ret))
end

# If you add a new error code that, when returned by COPT inside `optimize!`,
# should be treated as a TerminationStatus by MOI, to the global `Dict`
# below, then the rest of the code should pick up on this seamlessly.
const _ERROR_TO_STATUS = Dict{Cint,Tuple{MOI.TerminationStatusCode,String}}([
    # COPT return code => TerminationStatus
    COPT_RETCODE_MEMORY => (MOI.MEMORY_LIMIT, "Memory allocation failure."),
])

# Same as _check_ret, but deals with the `model.ret_optimize` machinery.
function _check_ret_optimize(model)
    if !haskey(_ERROR_TO_STATUS, model.ret_optimize)
        _check_ret(model, model.ret_optimize)
    end
    return
end

"""
    Optimizer(env::Union{Nothing, Env} = nothing)

Create a new Optimizer object.

You can share COPT `Env`s between models by passing an instance of `Env` as the
first argument.

Set optimizer attributes using `MOI.RawOptimizerAttribute` or
`JuMP.set_optimizer_atttribute`.

## Example

```julia
using JuMP, COPT
const env = COPT.Env()
model = JuMP.Model(() -> COPT.Optimizer(env)
set_optimizer_attribute(model, "LogToConsole", 0)
```

## `COPT.PassNames`

By default, variable and constraint names are stored in the MOI wrapper, but are
_not_ passed to the inner COPT model object because doing so can lead to a large
performance degradation. The downside of not passing names is that various log
messages from COPT will report names like constraint "R1" and variable "C2"
instead of their actual names. You can change this behavior using
`COPT.PassNames` to force COPT.jl to pass variable and constraint names to the
inner COPT model object:

```julia
using JuMP, COPT
model = JuMP.Model(COPT.Optimizer)
set_optimizer_attribute(model, COPT.PassNames(), true)
```
"""
mutable struct Optimizer <: MOI.AbstractOptimizer
    # The low-level COPT model.
    prob::Ptr{copt_prob}
    env::Env

    # Model name, not used by COPT.
    name::String

    # A flag to keep track of MOI.Silent, which over-rides the OutputFlag
    # parameter.
    silent::Bool

    variable_primal::Union{Nothing,Vector{Float64}}

    # Helpers to remember what objective is currently stored in the model.
    objective_type::_ObjectiveType
    objective_sense::Union{Nothing,MOI.OptimizationSense}

    # A mapping from the MOI.VariableIndex to the COPT column. _VariableInfo
    # also stores some additional fields like what bounds have been added, the
    # variable type, and the names of VariableIndex-in-Set constraints.
    variable_info::CleverDicts.CleverDict{MOI.VariableIndex,_VariableInfo}

    # An index that is incremented for each new constraint (regardless of type).
    # We can check if a constraint is valid by checking if it is in the correct
    # xxx_constraint_info. We should _not_ reset this to zero, since then new
    # constraints cannot be distinguished from previously created ones.
    last_constraint_index::Int
    # ScalarAffineFunction{Float64}-in-Set storage.
    affine_constraint_info::Dict{Int,_ConstraintInfo}
    # ScalarQuadraticFunction{Float64}-in-Set storage.
    quadratic_constraint_info::Dict{Int,_ConstraintInfo}
    # VectorOfVariables-in-Set storage.
    sos_constraint_info::Dict{Int,_ConstraintInfo}
    # VectorAffineFunction-in-Set storage.
    # the function info is also stored in the dict
    indicator_constraint_info::Dict{
        Int,
        Tuple{_ConstraintInfo,MOI.VectorAffineFunction{Float64}},
    }
    # Note: we do not have a VariableIndex_constraint_info dictionary. Instead,
    # data associated with these constraints are stored in the _VariableInfo
    # objects.

    # Mappings from variable and constraint names to their indices. These are
    # lazily built on-demand, so most of the time, they are `nothing`.
    name_to_variable::Union{
        Nothing,
        Dict{String,Union{Nothing,MOI.VariableIndex}},
    }
    name_to_constraint_index::Union{
        Nothing,
        Dict{String,Union{Nothing,MOI.ConstraintIndex}},
    }

    # COPT does detect when it needs more memory than it is available, but
    # returns an error code instead of setting the termination status (like it
    # does for the time limit). For convenience, and homogeinity with other
    # solvers, we save the code obtained inside `_optimize!` in `ret_optimize`,
    # and do not throw an exception case it should be interpreted as a
    # termination status. Then, when/if the termination status is queried, we
    # may override the result taking into account the `ret_optimize` field.
    ret_optimize::Cint
    solved_as_mip::Bool

    has_primal_certificate::Bool
    has_dual_certificate::Bool
    certificate::Vector{Float64}

    solve_time::Float64

    conflict::Any # ::Union{Nothing, ConflictRefinerData}

    # For more information on why `pass_names` is necessary, read:
    # https://github.com/jump-dev/CPLEX.jl/issues/392
    # The underlying problem is that we observed that add_variable, then set
    # VariableName then add_variable (i.e., what CPLEX in direct-mode does) is
    # faster than adding variable in batch then setting names in batch (i.e.,
    # what default_copy_to does). If implementing MOI.copy_to, you should take
    # this into consideration.
    pass_names::Bool

    function Optimizer(env::Union{Nothing,Env} = nothing)
        model = new()
        model.prob = C_NULL
        model.env = env === nothing ? Env() : env
        model.name = ""
        model.silent = false
        model.variable_primal = nothing

        model.variable_info =
            CleverDicts.CleverDict{MOI.VariableIndex,_VariableInfo}()
        model.affine_constraint_info = Dict{Int,_ConstraintInfo}()
        model.quadratic_constraint_info = Dict{Int,_ConstraintInfo}()
        model.sos_constraint_info = Dict{Int,_ConstraintInfo}()
        model.indicator_constraint_info =
            Dict{Int,Tuple{_ConstraintInfo,MOI.VectorAffineFunction{Float64}}}()
        model.certificate = Float64[]
        model.pass_names = false
        MOI.empty!(model)
        finalizer(model) do m
            ret = COPT_DeleteProb(Ref(m.prob))
            _check_ret(m, ret)
            m.env.attached_models -= 1
            if env === nothing
                # We created this env. Finalize it now
                finalize(m.env)
            elseif m.env.finalize_called && m.env.attached_models == 0
                # We delayed finalizing `m.env` earlier because there were still
                # models attached. Finalize it now.
                COPT_DeleteEnv(Ref(m.env.ptr))
                m.env.ptr = C_NULL
            end
        end
        return model
    end
end

"""
    ConeOptimizer(env::Union{Nothing, Env} = nothing)

Create a new ConeOptimizer object. ConeOptimizer must be used in place of
Optimizer for semi-definite programming (SDP) problems; i.e. for models with
matrix variables.

You can share COPT `Env`s between models by passing an instance of `Env` as the
first argument.

Set optimizer attributes using `MOI.RawOptimizerAttribute` or
`JuMP.set_optimizer_atttribute`.

## Example

```julia
using JuMP, COPT
const env = COPT.Env()
model = JuMP.Model(() -> COPT.ConeOptimizer(env)
set_optimizer_attribute(model, "LogToConsole", 0)
```

## `COPT.PassNames`

By default, variable and constraint names are stored in the MOI wrapper, but are
_not_ passed to the inner COPT model object because doing so can lead to a large
performance degradation. The downside of not passing names is that various log
messages from COPT will report names like constraint "R1" and variable "C2"
instead of their actual names. You can change this behavior using
`COPT.PassNames` to force COPT.jl to pass variable and constraint names to the
inner COPT model object:

```julia
using JuMP, COPT
model = JuMP.Model(COPT.ConeOptimizer)
set_optimizer_attribute(model, COPT.PassNames(), true)
```
"""
mutable struct ConeOptimizer <: MOI.AbstractOptimizer
    # The low-level COPT model.
    prob::Ptr{copt_prob}
    env::Env

    # Model name, not used by COPT.
    name::String

    # A flag to keep track of MOI.Silent, which over-rides the OutputFlag
    # parameter.
    silent::Bool

    cones::Union{Nothing,Cones{Float64}}
    solution::Union{Nothing,ConeSolution}

    # COPT does detect when it needs more memory than it is available, but
    # returns an error code instead of setting the termination status (like it
    # does for the time limit). For convenience, and homogeinity with other
    # solvers, we save the code obtained inside `_optimize!` in `ret_optimize`,
    # and do not throw an exception case it should be interpreted as a
    # termination status. Then, when/if the termination status is queried, we
    # may override the result taking into account the `ret_optimize` field.
    ret_optimize::Cint
    solve_time::Float64

    # For more information on why `pass_names` is necessary, read:
    # https://github.com/jump-dev/CPLEX.jl/issues/392
    # The underlying problem is that we observed that add_variable, then set
    # VariableName then add_variable (i.e., what CPLEX in direct-mode does) is
    # faster than adding variable in batch then setting names in batch (i.e.,
    # what default_copy_to does). If implementing MOI.copy_to, you should take
    # this into consideration.
    pass_names::Bool

    function ConeOptimizer(env::Union{Nothing,Env} = nothing)
        model = new()
        model.prob = C_NULL
        model.env = env === nothing ? Env() : env
        model.name = ""
        model.silent = false
        model.pass_names = false
        MOI.empty!(model)
        finalizer(model) do m
            ret = COPT_DeleteProb(Ref(m.prob))
            _check_ret(m, ret)
            m.env.attached_models -= 1
            if env === nothing
                # We created this env. Finalize it now
                finalize(m.env)
            elseif m.env.finalize_called && m.env.attached_models == 0
                # We delayed finalizing `m.env` earlier because there were still
                # models attached. Finalize it now.
                COPT_DeleteEnv(Ref(m.env.ptr))
                m.env.ptr = C_NULL
            end
        end
        return model
    end
end

function MOI.get(::ConeOptimizer, ::MOI.Bridges.ListOfNonstandardBridges)
    return [ReorderedPSDConeBridge{Cdouble}]
end

function _check_ret(model::Union{Optimizer,ConeOptimizer}, ret::Cint)
    return _check_ret(model.env, ret)
end

Base.show(io::IO, model::Union{Optimizer,ConeOptimizer}) = show(io, model.prob)

function _reset_prob!(model::Union{Optimizer,ConeOptimizer})
    if model.prob != C_NULL
        # Load an empty model into the COPT problem. This resets the problem
        # while keeping the parameters.
        ret = COPT_LoadProb(
            model.prob,
            0,
            0,
            COPT_MINIMIZE,
            0.0,
            C_NULL,
            C_NULL,
            C_NULL,
            C_NULL,
            C_NULL,
            C_NULL,
            C_NULL,
            C_NULL,
            C_NULL,
            C_NULL,
            C_NULL,
            C_NULL,
            C_NULL,
        )
        _check_ret(model.env, ret)
    else
        # Open a new problem.
        p_ptr = Ref{Ptr{copt_prob}}(C_NULL)
        ret = COPT_CreateProb(model.env.ptr, p_ptr)
        _check_ret(model.env, ret)
        model.prob = p_ptr[]
        model.env.attached_models += 1
        if model.silent
            MOI.set(model, MOI.RawOptimizerAttribute("LogToConsole"), 0)
        else
            MOI.set(model, MOI.RawOptimizerAttribute("LogToConsole"), 1)
        end
    end
end

function MOI.empty!(model::Optimizer)
    _reset_prob!(model)
    model.name = ""
    model.objective_type = _UNSET_OBJECTIVE
    model.objective_sense = nothing
    empty!(model.variable_info)
    empty!(model.affine_constraint_info)
    empty!(model.quadratic_constraint_info)
    empty!(model.sos_constraint_info)
    model.name_to_variable = nothing
    model.name_to_constraint_index = nothing
    model.ret_optimize = Cint(0)
    model.solved_as_mip = false
    empty!(model.certificate)
    model.has_primal_certificate = false
    model.has_dual_certificate = false
    model.solve_time = NaN
    model.conflict = nothing
    model.variable_primal = nothing
    return
end

function MOI.is_empty(model::Optimizer)
    model.name != "" && return false
    model.objective_type != _UNSET_OBJECTIVE && return false
    model.objective_sense !== nothing && return false
    !isempty(model.variable_info) && return false
    length(model.affine_constraint_info) != 0 && return false
    length(model.quadratic_constraint_info) != 0 && return false
    length(model.sos_constraint_info) != 0 && return false
    model.name_to_variable !== nothing && return false
    model.name_to_constraint_index !== nothing && return false
    model.ret_optimize !== Cint(0) && return false
    model.solved_as_mip && return false
    return true
end

function MOI.empty!(model::ConeOptimizer)
    _reset_prob!(model)
    model.name = ""
    model.cones = nothing
    model.solution = nothing
    model.ret_optimize = Cint(0)
    model.solve_time = NaN
    return
end

function MOI.is_empty(model::ConeOptimizer)
    model.name != "" && return false
    model.cones != nothing && return false
    model.solution != nothing && return false
    model.ret_optimize !== Cint(0) && return false
    return true
end

"""
    PassNames() <: MOI.AbstractOptimizerAttribute

An optimizer attribute to control whether COPT.jl should pass names to the
inner COPT model object. See the docstring of `COPT.Optimizer` for more
information.
"""
struct PassNames <: MOI.AbstractOptimizerAttribute end

function MOI.set(
    model::Union{Optimizer,ConeOptimizer},
    ::PassNames,
    value::Bool,
)
    model.pass_names = value
    return
end

MOI.get(::Union{Optimizer,ConeOptimizer}, ::MOI.SolverName) = "COPT"

function MOI.get(::Union{Optimizer,ConeOptimizer}, ::MOI.SolverVersion)
    return string(_COPT_VERSION)
end

function MOI.default_cache(::ConeOptimizer, ::Type{Float64})
    return MOI.Utilities.UniversalFallback(OptimizerCache())
end

function MOI.supports(
    ::Optimizer,
    ::MOI.ObjectiveFunction{F},
) where {
    F<:Union{
        MOI.VariableIndex,
        MOI.ScalarAffineFunction{Float64},
        MOI.ScalarQuadraticFunction{Float64},
    },
}
    return true
end

function MOI.supports_constraint(
    ::Optimizer,
    ::Type{MOI.VariableIndex},
    ::Type{F},
) where {
    F<:Union{
        MOI.EqualTo{Float64},
        MOI.LessThan{Float64},
        MOI.GreaterThan{Float64},
        MOI.Interval{Float64},
        MOI.ZeroOne,
        MOI.Integer,
    },
}
    return true
end

function MOI.supports_constraint(
    ::Optimizer,
    ::Type{MOI.VectorOfVariables},
    ::Type{F},
) where {F<:Union{MOI.SOS1{Float64},MOI.SOS2{Float64},MOI.SecondOrderCone}}
    return true
end

# ScalarAffineFunction-in-Interval is not supported by COPT.jl because COPT.jl
# was adopted from CPLEX.jl and CPLEX does not support ranged rows (without
# introducing slack variables). COPT does support ranged rows directly, so we
# actually could allow ScalarAffineFunction-in-Interval constraints.

function MOI.supports_constraint(
    ::Optimizer,
    ::Type{MOI.ScalarAffineFunction{Float64}},
    ::Type{F},
) where {
    F<:Union{
        MOI.EqualTo{Float64},
        MOI.LessThan{Float64},
        MOI.GreaterThan{Float64},
    },
}
    return true
end

function MOI.supports_constraint(
    ::Optimizer,
    ::Type{MOI.ScalarQuadraticFunction{Float64}},
    ::Type{F},
) where {F<:Union{MOI.LessThan{Float64},MOI.GreaterThan{Float64}}}
    # Note: COPT does not support quadratic equality constraints.
    return true
end

MOI.supports(::Optimizer, ::MOI.VariableName, ::Type{MOI.VariableIndex}) = true
function MOI.supports(
    ::Optimizer,
    ::MOI.ConstraintName,
    ::Type{<:MOI.ConstraintIndex},
)
    return true
end

function MOI.supports(
    ::ConeOptimizer,
    ::MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}},
)
    return true
end

function MOI.supports_constraint(
    ::ConeOptimizer,
    ::Type{MOI.VectorAffineFunction{Float64}},
    ::Type{
        <:Union{
            MOI.Zeros,
            MOI.Nonnegatives,
            MOI.SecondOrderCone,
            ReorderedPSDCone,
        },
    },
)
    return true
end

MOI.supports(::Union{Optimizer,ConeOptimizer}, ::MOI.Name) = true
MOI.supports(::Union{Optimizer,ConeOptimizer}, ::MOI.Silent) = true
MOI.supports(::Union{Optimizer,ConeOptimizer}, ::MOI.NumberOfThreads) = true
MOI.supports(::Union{Optimizer,ConeOptimizer}, ::MOI.TimeLimitSec) = true
MOI.supports(::Union{Optimizer,ConeOptimizer}, ::MOI.ObjectiveSense) = true
function MOI.supports(
    ::Union{Optimizer,ConeOptimizer},
    ::MOI.RawOptimizerAttribute,
)
    return true
end

"""
    _search_param_attr(model::Union{Optimizer,ConeOptimizer}, name::AbstractString) -> Int

Returns the type of a COPT parameter or attribute, given its name.
-1: unknown
 0: double parameter
 1: int parameter
 2: double attribute
 3: int attribute
"""
function _search_param_attr(
    model::Union{Optimizer,ConeOptimizer},
    name::AbstractString,
)
    # Use undocumented COPT function
    # int COPT_SearchParamAttr(copt_prob* prob, const char* name, int* p_type)
    p_type = Ref{Cint}()
    ret = ccall(
        (:COPT_SearchParamAttr, libcopt),
        Cint,
        (Ptr{copt_prob}, Cstring, Ptr{Cint}),
        model.prob,
        name,
        p_type,
    )
    _check_ret(model, ret)
    return convert(Int, p_type[])
end

function _copt_get_int_attr(model::Union{Optimizer,ConeOptimizer}, name::String)
    p_value = Ref{Cint}()
    ret = COPT_GetIntAttr(model.prob, name, p_value)
    _check_ret(model, ret)
    return p_value[]
end

function _copt_get_dbl_attr(model::Union{Optimizer,ConeOptimizer}, name::String)
    p_value = Ref{Cdouble}()
    ret = COPT_GetDblAttr(model.prob, name, p_value)
    _check_ret(model, ret)
    return p_value[]
end

function _copt_get_col_info(model::Optimizer, name::String, copt_col)
    p_value = Ref{Cdouble}()
    ret = COPT_GetColInfo(model.prob, name, 1, Cint[copt_col], p_value)
    _check_ret(model, ret)
    return p_value[]
end

function _copt_get_col_info(model::Optimizer, name::String)
    num_col = length(model.variable_info)
    values = Array{Cdouble}(undef, num_col)
    ret = COPT_GetColInfo(model.prob, name, num_col, C_NULL, values)
    _check_ret(model, ret)
    return values
end

function _copt_get_col_lower(model::Optimizer, copt_col::Cint)
    lb = _copt_get_col_info(model, "LB", copt_col)
    return lb == -COPT_INFINITY ? -Inf : lb
end

function _copt_get_col_upper(model::Optimizer, copt_col::Cint)
    ub = _copt_get_col_info(model, "UB", copt_col)
    return ub == COPT_INFINITY ? Inf : ub
end

function _copt_set_col_lower(model::Optimizer, copt_col::Cint, value)
    if value == -Inf
        value = -COPT_INFINITY
    end
    ret = COPT_SetColLower(model.prob, 1, [copt_col], Cdouble[value])
    return _check_ret(model, ret)
end

function _copt_set_col_upper(model::Optimizer, copt_col::Cint, value)
    if value == +Inf
        value = +COPT_INFINITY
    end
    ret = COPT_SetColUpper(model.prob, 1, [copt_col], Cdouble[value])
    return _check_ret(model, ret)
end

function _copt_get_row_info(model::Optimizer, name::String, copt_row)
    p_value = Ref{Cdouble}()
    ret = COPT_GetRowInfo(model.prob, name, 1, [copt_row], p_value)
    _check_ret(model, ret)
    return p_value[]
end

function _copt_get_row_lower(model::Optimizer, copt_row::Cint)
    lb = _copt_get_row_info(model, "LB", copt_row)
    return lb == -COPT_INFINITY ? -Inf : lb
end

function _copt_get_row_upper(model::Optimizer, copt_row::Cint)
    ub = _copt_get_row_info(model, "UB", copt_row)
    return ub == COPT_INFINITY ? Inf : ub
end

function _copt_set_row_lower(model::Optimizer, copt_row::Cint, value)
    if value == -Inf
        value = -COPT_INFINITY
    end
    ret = COPT_SetRowLower(model.prob, 1, [copt_row], Cdouble[value])
    return _check_ret(model, ret)
end

function _copt_set_row_upper(model::Optimizer, copt_row::Cint, value)
    if value == +Inf
        value = +COPT_INFINITY
    end
    ret = COPT_SetRowUpper(model.prob, 1, [copt_row], Cdouble[value])
    return _check_ret(model, ret)
end

function _copt_get_row(model::Optimizer, copt_row::Cint)
    p_reqsize = Ref{Cint}()
    ret = COPT_GetRows(
        model.prob,
        1,
        [copt_row],
        C_NULL,
        C_NULL,
        C_NULL,
        C_NULL,
        0,
        p_reqsize,
    )
    _check_ret(model, ret)
    num_elem = p_reqsize[]
    rowbeg = Array{Cint}(undef, 1)
    rowcnt = Array{Cint}(undef, 1)
    columns = Array{Cint}(undef, num_elem)
    coefficients = Array{Cdouble}(undef, num_elem)
    ret = COPT_GetRows(
        model.prob,
        1,
        [copt_row],
        rowbeg,
        rowcnt,
        columns,
        coefficients,
        num_elem,
        p_reqsize,
    )
    _check_ret(model, ret)
    return columns, coefficients
end

function _copt_getqconstr(model::Optimizer, copt_row::Cint)
    p_reqsize = Ref{Cint}()
    p_qreqsize = Ref{Cint}()
    ret = COPT_GetQConstr(
        model.prob,
        copt_row,
        C_NULL,
        C_NULL,
        C_NULL,
        0,
        p_qreqsize,
        C_NULL,
        C_NULL,
        C_NULL,
        C_NULL,
        0,
        p_reqsize,
    )
    _check_ret(model, ret)
    num_elem = p_reqsize[]
    num_qelem = p_qreqsize[]
    linind = Vector{Cint}(undef, num_elem)
    linval = Vector{Cdouble}(undef, num_elem)
    quadrow = Vector{Cint}(undef, num_qelem)
    quadcol = Vector{Cint}(undef, num_qelem)
    quadval = Vector{Cdouble}(undef, num_qelem)
    ret = COPT_GetQConstr(
        model.prob,
        copt_row,
        quadrow,
        quadcol,
        quadval,
        num_qelem,
        C_NULL,
        linind,
        linval,
        C_NULL,
        C_NULL,
        num_elem,
        C_NULL,
    )
    _check_ret(model, ret)
    return linind, linval, quadrow, quadcol, quadval
end

function _copt_getsos(model::Optimizer, copt_row::Cint)
    p_reqsize = Ref{Cint}()
    ret = COPT_GetSOSs(
        model.prob,
        1,
        [copt_row],
        C_NULL,
        C_NULL,
        C_NULL,
        C_NULL,
        C_NULL,
        0,
        p_reqsize,
    )
    _check_ret(model, ret)
    num_elem = p_reqsize[]
    sostype = Vector{Cint}(undef, 1)
    sosbeg = Vector{Cint}(undef, 1)
    soscnt = Vector{Cint}(undef, 1)
    sosind = Vector{Cint}(undef, num_elem)
    soswt = Vector{Cdouble}(undef, num_elem)
    ret = COPT_GetSOSs(
        model.prob,
        1,
        [copt_row],
        sostype,
        sosbeg,
        soscnt,
        sosind,
        soswt,
        num_elem,
        C_NULL,
    )
    _check_ret(model, ret)
    return sosind, soswt
end

function MOI.set(
    model::Union{Optimizer,ConeOptimizer},
    param::MOI.RawOptimizerAttribute,
    value,
)
    param_type = _search_param_attr(model, param.name)
    if param_type == 0
        ret = COPT_SetDblParam(model.prob, param.name, value)
        _check_ret(model, ret)
    elseif param_type == 1
        ret = COPT_SetIntParam(model.prob, param.name, value)
        _check_ret(model, ret)
    else
        throw(MOI.UnsupportedAttribute(param))
    end
end

function MOI.get(
    model::Union{Optimizer,ConeOptimizer},
    param::MOI.RawOptimizerAttribute,
)
    param_type = _search_param_attr(model, param.name)
    if param_type == 0
        p_value = Ref{Cdouble}()
        ret = COPT_GetDblParam(model.prob, param.name, p_value)
        _check_ret(model, ret)
        return p_value[]
    elseif param_type == 1
        p_value = Ref{Cint}()
        ret = COPT_GetIntParam(model.prob, param.name, p_value)
        _check_ret(model, ret)
        return p_value[]
    else
        throw(MOI.UnsupportedAttribute(param))
    end
end

function MOI.set(
    model::Union{Optimizer,ConeOptimizer},
    ::MOI.TimeLimitSec,
    limit::Real,
)
    MOI.set(model, MOI.RawOptimizerAttribute("TimeLimit"), limit)
    return
end

function MOI.get(model::Union{Optimizer,ConeOptimizer}, ::MOI.TimeLimitSec)
    return MOI.get(model, MOI.RawOptimizerAttribute("TimeLimit"))
end

MOI.supports_incremental_interface(::Optimizer) = true

# !!! info
#     If modifying this function, read the comment in the defintion of Optimizer
#     about the need for `pass_names`.
function MOI.copy_to(dest::Optimizer, src::MOI.ModelLike)
    return MOI.Utilities.default_copy_to(dest, src)
end

function MOI.get(model::Optimizer, ::MOI.ListOfVariableAttributesSet)
    ret = MOI.AbstractVariableAttribute[]
    found_name, found_start = false, false
    for info in values(model.variable_info)
        if !found_name && !isempty(info.name)
            push!(ret, MOI.VariableName())
            found_name = true
        end
        if !found_start && info.start !== nothing
            push!(ret, MOI.VariablePrimalStart())
            found_start = true
        end
        if found_start && found_name
            return ret
        end
    end
    return ret
end

function MOI.get(model::Optimizer, ::MOI.ListOfModelAttributesSet)
    attributes = MOI.AbstractModelAttribute[]
    if model.objective_sense !== nothing
        push!(attributes, MOI.ObjectiveSense())
    end
    if model.objective_type != _UNSET_OBJECTIVE
        F = MOI.get(model, MOI.ObjectiveFunctionType())
        push!(attributes, MOI.ObjectiveFunction{F}())
    end
    if MOI.get(model, MOI.Name()) != ""
        push!(attributes, MOI.Name())
    end
    return attributes
end

function MOI.get(::Optimizer, ::MOI.ListOfConstraintAttributesSet)
    return MOI.AbstractConstraintAttribute[MOI.ConstraintName()]
end

function MOI.get(
    ::Optimizer,
    ::MOI.ListOfConstraintAttributesSet{MOI.VariableIndex},
)
    return MOI.AbstractConstraintAttribute[]
end

function _indices_and_coefficients(
    indices::AbstractVector{Cint},
    coefficients::AbstractVector{Float64},
    model::Optimizer,
    f::MOI.ScalarAffineFunction{Float64},
)
    for (i, term) in enumerate(f.terms)
        indices[i] = Cint(column(model, term.variable) - 1)
        coefficients[i] = term.coefficient
    end
    return indices, coefficients
end

function _indices_and_coefficients(
    model::Optimizer,
    f::MOI.ScalarAffineFunction{Float64},
)
    f_canon = MOI.Utilities.canonical(f)
    nnz = length(f_canon.terms)
    indices = Vector{Cint}(undef, nnz)
    coefficients = Vector{Float64}(undef, nnz)
    _indices_and_coefficients(indices, coefficients, model, f_canon)
    return indices, coefficients
end

function _indices_and_coefficients(
    I::AbstractVector{Cint},
    J::AbstractVector{Cint},
    V::AbstractVector{Float64},
    indices::AbstractVector{Cint},
    coefficients::AbstractVector{Float64},
    model::Optimizer,
    f::MOI.ScalarQuadraticFunction,
)
    for (i, term) in enumerate(f.quadratic_terms)
        I[i] = Cint(column(model, term.variable_1) - 1)
        J[i] = Cint(column(model, term.variable_2) - 1)
        V[i] = term.coefficient
        # COPT returns a list of terms. MOI requires 0.5 x' Q x. So, to get
        # from
        #   COPT -> MOI => multiply diagonals by 2.0
        #   MOI -> COPT => multiply diagonals by 0.5
        # Example: 2x^2 + x*y + y^2
        #   |x y| * |a b| * |x| = |ax+by bx+cy| * |x| = 0.5ax^2 + bxy + 0.5cy^2
        #           |b c|   |y|                   |y|
        #   COPT needs: (I, J, V) = ([0, 0, 1], [0, 1, 1], [2, 1, 1])
        #   MOI needs:
        #     [SQT(4.0, x, x), SQT(1.0, x, y), SQT(2.0, y, y)]
        if I[i] == J[i]
            V[i] *= 0.5
        end
    end
    for (i, term) in enumerate(f.affine_terms)
        indices[i] = Cint(column(model, term.variable) - 1)
        coefficients[i] = term.coefficient
    end
    return
end

function _indices_and_coefficients(
    model::Optimizer,
    f::MOI.ScalarQuadraticFunction,
)
    f_canon = MOI.Utilities.canonical(f)
    nnz_quadratic = length(f_canon.quadratic_terms)
    nnz_affine = length(f_canon.affine_terms)
    I = Vector{Cint}(undef, nnz_quadratic)
    J = Vector{Cint}(undef, nnz_quadratic)
    V = Vector{Float64}(undef, nnz_quadratic)
    indices = Vector{Cint}(undef, nnz_affine)
    coefficients = Vector{Float64}(undef, nnz_affine)
    _indices_and_coefficients(I, J, V, indices, coefficients, model, f_canon)
    return indices, coefficients, I, J, V
end

_sense_and_rhs(s::MOI.LessThan{Float64}) = (Cchar('L'), s.upper)
_sense_and_rhs(s::MOI.GreaterThan{Float64}) = (Cchar('G'), s.lower)
_sense_and_rhs(s::MOI.EqualTo{Float64}) = (Cchar('E'), s.value)

###
### Variables
###

# Short-cuts to return the _VariableInfo associated with an index.
function _info(model::Optimizer, key::MOI.VariableIndex)
    if haskey(model.variable_info, key)
        return model.variable_info[key]
    end
    return throw(MOI.InvalidIndex(key))
end

"""
    column(model::Optimizer, x::MOI.VariableIndex)

Return the 1-indexed column associated with `x`.

The C API requires 0-indexed columns.
"""
function column(model::Optimizer, x::MOI.VariableIndex)
    return _info(model, x).column
end

function column(model::Optimizer, x::Vector{MOI.VariableIndex})
    return [_info(model, xi).column for xi in x]
end

function MOI.add_variable(model::Optimizer)
    # Initialize `_VariableInfo` with a dummy `VariableIndex` and a column,
    # because we need `add_item` to tell us what the `VariableIndex` is.
    index = CleverDicts.add_item(
        model.variable_info,
        _VariableInfo(MOI.VariableIndex(0), 0),
    )
    info = _info(model, index)
    info.index = index
    info.column = length(model.variable_info)
    ret = COPT_AddCol(
        model.prob,
        0.0,
        0,
        C_NULL,
        C_NULL,
        COPT_CONTINUOUS,
        -COPT_INFINITY,
        COPT_INFINITY,
        "",
    )
    _check_ret(model, ret)
    return index
end

function MOI.add_variables(model::Optimizer, N::Int)
    ret = COPT_AddCols(
        model.prob,
        N,
        C_NULL,
        C_NULL,
        C_NULL,
        C_NULL,
        C_NULL,
        C_NULL,
        fill(-COPT_INFINITY, N),
        C_NULL,
        C_NULL,
    )
    _check_ret(model, ret)
    indices = Vector{MOI.VariableIndex}(undef, N)
    num_variables = length(model.variable_info)
    for i in 1:N
        # Initialize `_VariableInfo` with a dummy `VariableIndex` and a column,
        # because we need `add_item` to tell us what the `VariableIndex` is.
        index = CleverDicts.add_item(
            model.variable_info,
            _VariableInfo(MOI.VariableIndex(0), 0),
        )
        info = _info(model, index)
        info.index = index
        info.column = num_variables + i
        indices[i] = index
    end
    return indices
end

function MOI.is_valid(model::Optimizer, v::MOI.VariableIndex)
    return haskey(model.variable_info, v)
end

# Helper function used inside MOI.delete (vector version). Takes a list of
# numbers (MOI.VariableIndex) sorted by increasing values, return two lists
# representing the same set of numbers but in the form of intervals.
# Ex.: _intervalize([1, 3, 4, 5, 8, 10, 11]) -> ([1, 3, 8, 10], [1, 5, 8, 11])
function _intervalize(xs)
    starts, ends = empty(xs), empty(xs)
    for x in xs
        if isempty(starts) || x != last(ends) + 1
            push!(starts, x)
            push!(ends, x)
        else
            ends[end] = x
        end
    end

    return starts, ends
end

function MOI.delete(model::Optimizer, indices::Vector{<:MOI.VariableIndex})
    info = [_info(model, var_idx) for var_idx in indices]
    soc_idx = findfirst(e -> e.num_soc_constraints > 0, info)
    soc_idx !== nothing && throw(MOI.DeleteNotAllowed(indices[soc_idx]))
    sorted_del_cols = sort!(collect(i.column for i in info))
    starts, ends = _intervalize(sorted_del_cols)
    for ri in reverse(1:length(starts))
        col_list = convert(Vector{Cint}, (starts[ri]-1):(ends[ri]-1))
        ret = COPT_DelCols(model.prob, length(col_list), col_list)
        _check_ret(model, ret)
    end
    for var_idx in indices
        delete!(model.variable_info, var_idx)
    end
    # When the deleted variables are not contiguous, the main advantage of this
    # method is that the loop below is O(n*log(m)) instead of the O(m*n) of the
    # repeated application of single variable delete (n is the total number of
    # variables in the model, m is the number of deleted variables).
    for other_info in values(model.variable_info)
        # The trick here is: `searchsortedlast` returns, in O(log n), the
        # last index with a row smaller than `other_info.row`, over
        # `sorted_del_cols` this is the same as the number of rows deleted
        # before it, and how much its value need to be shifted.
        other_info.column -=
            searchsortedlast(sorted_del_cols, other_info.column)
    end
    model.name_to_variable = nothing
    # We throw away name_to_constraint_index so we will rebuild VariableIndex
    # constraint names without v.
    model.name_to_constraint_index = nothing
    return
end

function MOI.delete(model::Optimizer, v::MOI.VariableIndex)
    info = _info(model, v)
    if info.num_soc_constraints > 0
        throw(MOI.DeleteNotAllowed(v))
    end
    ret = COPT_DelCols(model.prob, 1, [Cint(info.column - 1)])
    _check_ret(model, ret)
    delete!(model.variable_info, v)
    for other_info in values(model.variable_info)
        if other_info.column > info.column
            other_info.column -= 1
        end
    end
    model.name_to_variable = nothing
    # We throw away name_to_constraint_index so we will rebuild VariableIndex
    # constraint names without v.
    model.name_to_constraint_index = nothing
    return
end

function MOI.get(model::Optimizer, ::Type{MOI.VariableIndex}, name::String)
    if model.name_to_variable === nothing
        _rebuild_name_to_variable(model)
    end
    if haskey(model.name_to_variable, name)
        variable = model.name_to_variable[name]
        if variable === nothing
            error("Duplicate variable name detected: $(name)")
        end
        return variable
    end
    return nothing
end

function _rebuild_name_to_variable(model::Optimizer)
    model.name_to_variable = Dict{String,Union{Nothing,MOI.VariableIndex}}()
    for (index, info) in model.variable_info
        if info.name == ""
            continue
        end
        if haskey(model.name_to_variable, info.name)
            model.name_to_variable[info.name] = nothing
        else
            model.name_to_variable[info.name] = index
        end
    end
    return
end

function MOI.get(model::Optimizer, ::MOI.VariableName, v::MOI.VariableIndex)
    return _info(model, v).name
end

function MOI.set(
    model::Optimizer,
    ::MOI.VariableName,
    v::MOI.VariableIndex,
    name::String,
)
    info = _info(model, v)
    if model.pass_names && info.name != name && isascii(name)
        ret = COPT_SetColNames(model.prob, 1, [Cint(info.column - 1)], [name])
        _check_ret(model, ret)
    end
    info.name = name
    model.name_to_variable = nothing
    return
end

###
### Objectives
###

function _zero_objective(model::Optimizer)
    ret = COPT_DelQuadObj(model.prob)
    _check_ret(model, ret)
    num_vars = length(model.variable_info)
    # COPT returns an error when calling COPT_SetColObj() with no columns.
    if num_vars > 0
        obj = zeros(Float64, num_vars)
        ret = COPT_SetColObj(model.prob, num_vars, C_NULL, obj)
        _check_ret(model, ret)
    end
    ret = COPT_SetObjConst(model.prob, 0.0)
    _check_ret(model, ret)
    return
end

function MOI.set(
    model::Optimizer,
    ::MOI.ObjectiveSense,
    sense::MOI.OptimizationSense,
)
    ret = if sense == MOI.MIN_SENSE
        COPT_SetObjSense(model.prob, COPT_MINIMIZE)
    elseif sense == MOI.MAX_SENSE
        COPT_SetObjSense(model.prob, COPT_MAXIMIZE)
    else
        @assert sense == MOI.FEASIBILITY_SENSE
        _zero_objective(model)
        COPT_SetObjSense(model.prob, COPT_MINIMIZE)
    end
    _check_ret(model, ret)
    model.objective_sense = sense
    return
end

function MOI.get(model::Optimizer, ::MOI.ObjectiveSense)
    return something(model.objective_sense, MOI.FEASIBILITY_SENSE)
end

function MOI.set(
    model::Optimizer,
    ::MOI.ObjectiveFunction{F},
    f::F,
) where {F<:MOI.VariableIndex}
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        convert(MOI.ScalarAffineFunction{Float64}, f),
    )
    model.objective_type = _SINGLE_VARIABLE
    return
end

function MOI.get(model::Optimizer, ::MOI.ObjectiveFunction{MOI.VariableIndex})
    obj = MOI.get(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    return convert(MOI.VariableIndex, obj)
end

function MOI.set(
    model::Optimizer,
    ::MOI.ObjectiveFunction{F},
    f::F,
) where {F<:MOI.ScalarAffineFunction{Float64}}
    num_vars = length(model.variable_info)
    if model.objective_type == _SCALAR_QUADRATIC
        # We need to zero out the existing quadratic objective.
        ret = COPT_DelQuadObj(model.prob)
        _check_ret(model, ret)
    end
    obj = zeros(Float64, num_vars)
    for term in f.terms
        col = column(model, term.variable)
        obj[col] += term.coefficient
    end
    ret = COPT_SetColObj(model.prob, num_vars, C_NULL, obj)
    _check_ret(model, ret)
    ret = COPT_SetObjConst(model.prob, f.constant)
    _check_ret(model, ret)
    model.objective_type = _SCALAR_AFFINE
    return
end

function MOI.get(
    model::Optimizer,
    ::MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}},
)
    if model.objective_type == _SCALAR_QUADRATIC
        error(
            "Unable to get objective function. Currently: $(model.objective_type).",
        )
    end
    dest = _copt_get_col_info(model, "Obj")
    terms = MOI.ScalarAffineTerm{Float64}[]
    for (index, info) in model.variable_info
        coefficient = dest[info.column]
        if !iszero(coefficient)
            push!(terms, MOI.ScalarAffineTerm(coefficient, index))
        end
    end
    constant = _copt_get_dbl_attr(model, "ObjConst")
    return MOI.ScalarAffineFunction(terms, constant)
end

function MOI.set(
    model::Optimizer,
    ::MOI.ObjectiveFunction{F},
    f::F,
) where {F<:MOI.ScalarQuadraticFunction{Float64}}
    a, b, I, J, V = _indices_and_coefficients(model, f)
    n = length(model.variable_info)
    obj = zeros(n)
    for (i, c) in zip(a, b)
        obj[i+1] += c
    end
    ret = COPT_SetColObj(model.prob, n, C_NULL, obj)
    _check_ret(model, ret)
    ret = COPT_SetObjConst(model.prob, f.constant)
    _check_ret(model, ret)
    COPT_SetQuadObj(model.prob, length(I), I, J, V)
    _check_ret(model, ret)
    model.objective_type = _SCALAR_QUADRATIC
    return
end

function MOI.get(
    model::Optimizer,
    ::MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}},
)
    dest = _copt_get_col_info(model, "Obj")
    terms = MOI.ScalarAffineTerm{Float64}[]
    for (index, info) in model.variable_info
        coefficient = dest[info.column]
        iszero(coefficient) && continue
        push!(terms, MOI.ScalarAffineTerm(coefficient, index))
    end
    constant = _copt_get_dbl_attr(model, "ObjConst")
    q_terms = MOI.ScalarQuadraticTerm{Float64}[]
    # COPT returns an error when calling COPT_GetQuadObj() and no quadratic
    # objective is available.
    has_qobj = _copt_get_int_attr(model, "HasQObj")
    if has_qobj != 0
        num_qnz = _copt_get_int_attr(model, "QElems")
        qrow = Array{Cint}(undef, num_qnz)
        qcol = Array{Cint}(undef, num_qnz)
        qval = Array{Float64}(undef, num_qnz)
        p_numqnz = Ref{Cint}()
        ret = COPT_GetQuadObj(model.prob, p_numqnz, qrow, qcol, qval)
        _check_ret(model, ret)
        @assert p_numqnz[] == length(qval)
        for (i, j, v) in zip(qrow, qcol, qval)
            if iszero(v)
                continue
            end
            # See note in `_indices_and_coefficients`.
            new_v = i == j ? 2v : v
            push!(
                q_terms,
                MOI.ScalarQuadraticTerm(
                    new_v,
                    model.variable_info[CleverDicts.LinearIndex(i + 1)].index,
                    model.variable_info[CleverDicts.LinearIndex(j + 1)].index,
                ),
            )
        end
    end
    return MOI.Utilities.canonical(
        MOI.ScalarQuadraticFunction(q_terms, terms, constant),
    )
end

function MOI.modify(
    model::Optimizer,
    ::MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}},
    chg::MOI.ScalarConstantChange{Float64},
)
    ret = COPT_SetObjConst(model.prob, chg.new_constant)
    _check_ret(model, ret)
    return
end

##
##  VariableIndex-in-Set constraints.
##

function _info(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.VariableIndex,<:Any},
)
    var_index = MOI.VariableIndex(c.value)
    if haskey(model.variable_info, var_index)
        return _info(model, var_index)
    end
    return throw(MOI.InvalidIndex(c))
end

"""
    column(model::Optimizer, c::MOI.ConstraintIndex{MOI.VariableIndex, <:Any})

Return the 1-indexed column associated with `c`.

The C API requires 0-indexed columns.
"""
function column(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.VariableIndex,<:Any},
)
    return _info(model, c).column
end

function MOI.is_valid(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.VariableIndex,MOI.LessThan{Float64}},
)
    if haskey(model.variable_info, MOI.VariableIndex(c.value))
        info = _info(model, c)
        return info.bound == _LESS_THAN || info.bound == _LESS_AND_GREATER_THAN
    end
    return false
end

function MOI.is_valid(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.VariableIndex,MOI.GreaterThan{Float64}},
)
    if haskey(model.variable_info, MOI.VariableIndex(c.value))
        info = _info(model, c)
        return info.bound == _GREATER_THAN ||
               info.bound == _LESS_AND_GREATER_THAN
    end
    return false
end

function MOI.is_valid(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.VariableIndex,MOI.Interval{Float64}},
)
    return haskey(model.variable_info, MOI.VariableIndex(c.value)) &&
           _info(model, c).bound == _INTERVAL
end

function MOI.is_valid(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.VariableIndex,MOI.EqualTo{Float64}},
)
    return haskey(model.variable_info, MOI.VariableIndex(c.value)) &&
           _info(model, c).bound == _EQUAL_TO
end

function MOI.is_valid(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.VariableIndex,MOI.ZeroOne},
)
    return haskey(model.variable_info, MOI.VariableIndex(c.value)) &&
           _info(model, c).type == COPT_BINARY
end

function MOI.is_valid(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.VariableIndex,MOI.Integer},
)
    return haskey(model.variable_info, MOI.VariableIndex(c.value)) &&
           _info(model, c).type == COPT_INTEGER
end

function MOI.get(
    model::Optimizer,
    ::MOI.ConstraintFunction,
    c::MOI.ConstraintIndex{MOI.VariableIndex,<:Any},
)
    MOI.throw_if_not_valid(model, c)
    return MOI.VariableIndex(c.value)
end

function MOI.set(
    model::Optimizer,
    ::MOI.ConstraintFunction,
    c::MOI.ConstraintIndex{MOI.VariableIndex,<:Any},
    ::MOI.VariableIndex,
)
    return throw(MOI.SettingVariableIndexNotAllowed())
end

_bounds(s::MOI.GreaterThan{Float64}) = (s.lower, nothing)
_bounds(s::MOI.LessThan{Float64}) = (nothing, s.upper)
_bounds(s::MOI.EqualTo{Float64}) = (s.value, s.value)
_bounds(s::MOI.Interval{Float64}) = (s.lower, s.upper)

function _throw_if_existing_lower(
    bound::_BoundType,
    var_type::Cchar,
    new_set::Type{<:MOI.AbstractSet},
    variable::MOI.VariableIndex,
)
    existing_set = if bound == _LESS_AND_GREATER_THAN || bound == _GREATER_THAN
        MOI.GreaterThan{Float64}
    elseif bound == _INTERVAL
        MOI.Interval{Float64}
    elseif bound == _EQUAL_TO
        MOI.EqualTo{Float64}
    else
        nothing  # Also covers `_NONE` and `_LESS_THAN`.
    end
    if existing_set !== nothing
        throw(MOI.LowerBoundAlreadySet{existing_set,new_set}(variable))
    end
end

function _throw_if_existing_upper(
    bound::_BoundType,
    var_type::Cchar,
    new_set::Type{<:MOI.AbstractSet},
    variable::MOI.VariableIndex,
)
    existing_set = if bound == _LESS_AND_GREATER_THAN || bound == _LESS_THAN
        MOI.LessThan{Float64}
    elseif bound == _INTERVAL
        MOI.Interval{Float64}
    elseif bound == _EQUAL_TO
        MOI.EqualTo{Float64}
    else
        nothing  # Also covers `_NONE` and `_GREATER_THAN`.
    end
    if existing_set !== nothing
        throw(MOI.UpperBoundAlreadySet{existing_set,new_set}(variable))
    end
end

function MOI.add_constraint(
    model::Optimizer,
    f::MOI.VariableIndex,
    s::S,
) where {S<:_SCALAR_SETS}
    info = _info(model, f)
    if S <: MOI.LessThan{Float64}
        _throw_if_existing_upper(info.bound, info.type, S, f)
        info.bound =
            info.bound == _GREATER_THAN ? _LESS_AND_GREATER_THAN : _LESS_THAN
    elseif S <: MOI.GreaterThan{Float64}
        _throw_if_existing_lower(info.bound, info.type, S, f)
        info.bound =
            info.bound == _LESS_THAN ? _LESS_AND_GREATER_THAN : _GREATER_THAN
    elseif S <: MOI.EqualTo{Float64}
        _throw_if_existing_lower(info.bound, info.type, S, f)
        _throw_if_existing_upper(info.bound, info.type, S, f)
        info.bound = _EQUAL_TO
    else
        @assert S <: MOI.Interval{Float64}
        _throw_if_existing_lower(info.bound, info.type, S, f)
        _throw_if_existing_upper(info.bound, info.type, S, f)
        info.bound = _INTERVAL
    end
    index = MOI.ConstraintIndex{MOI.VariableIndex,typeof(s)}(f.value)
    MOI.set(model, MOI.ConstraintSet(), index, s)
    return index
end

function MOI.add_constraints(
    model::Optimizer,
    f::Vector{MOI.VariableIndex},
    s::Vector{S},
) where {S<:_SCALAR_SETS}
    for fi in f
        info = _info(model, fi)
        if S <: MOI.LessThan{Float64}
            _throw_if_existing_upper(info.bound, info.type, S, fi)
            info.bound =
                info.bound == _GREATER_THAN ? _LESS_AND_GREATER_THAN :
                _LESS_THAN
        elseif S <: MOI.GreaterThan{Float64}
            _throw_if_existing_lower(info.bound, info.type, S, fi)
            info.bound =
                info.bound == _LESS_THAN ? _LESS_AND_GREATER_THAN :
                _GREATER_THAN
        elseif S <: MOI.EqualTo{Float64}
            _throw_if_existing_lower(info.bound, info.type, S, fi)
            _throw_if_existing_upper(info.bound, info.type, S, fi)
            info.bound = _EQUAL_TO
        else
            @assert S <: MOI.Interval{Float64}
            _throw_if_existing_lower(info.bound, info.type, S, fi)
            _throw_if_existing_upper(info.bound, info.type, S, fi)
            info.bound = _INTERVAL
        end
    end
    indices =
        [MOI.ConstraintIndex{MOI.VariableIndex,eltype(s)}(fi.value) for fi in f]
    _set_bounds(model, indices, s)
    return indices
end

function _set_bounds(
    model::Optimizer,
    indices::Vector{MOI.ConstraintIndex{MOI.VariableIndex,S}},
    sets::Vector{S},
) where {S}
    lower_columns, lower_values = Cint[], Float64[]
    upper_columns, upper_values = Cint[], Float64[]
    for (c, s) in zip(indices, sets)
        lower, upper = _bounds(s)
        info = _info(model, c)
        if lower !== nothing
            push!(lower_columns, Cint(info.column - 1))
            push!(lower_values, lower)
        end
        if upper !== nothing
            push!(upper_columns, Cint(info.column - 1))
            push!(upper_values, upper)
        end
    end
    if length(lower_columns) > 0
        ret = COPT_SetColLower(
            model.prob,
            length(lower_columns),
            lower_columns,
            lower_values,
        )
        _check_ret(model, ret)
    end
    if length(upper_columns) > 0
        ret = COPT_SetColUpper(
            model.prob,
            length(upper_columns),
            upper_columns,
            upper_values,
        )
        _check_ret(model, ret)
    end
end

function MOI.delete(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.VariableIndex,MOI.LessThan{Float64}},
)
    MOI.throw_if_not_valid(model, c)
    info = _info(model, c)
    _set_variable_upper_bound(model, info, Inf)
    if info.bound == _LESS_AND_GREATER_THAN
        info.bound = _GREATER_THAN
    else
        info.bound = _NONE
    end
    model.name_to_constraint_index = nothing
    return
end

"""
    _set_variable_lower_bound(model, info, value)

This function is used to indirectly set the lower bound of a variable.

We need to do it this way to account for potential lower bounds of 0.0 added by
VectorOfVariables-in-SecondOrderCone constraints.

See also `_get_variable_lower_bound`.
"""
function _set_variable_lower_bound(model, info, value)
    if info.num_soc_constraints == 0
        # No SOC constraints, set directly.
        @assert isnan(info.lower_bound_if_soc)
        _copt_set_col_lower(model, Cint(info.column - 1), value)
    elseif value >= 0.0
        # Regardless of whether there are SOC constraints, this is a valid bound
        # for the SOC constraint and should over-ride any previous bounds.
        info.lower_bound_if_soc = NaN
        _copt_set_col_lower(model, Cint(info.column - 1), value)
    elseif isnan(info.lower_bound_if_soc)
        # Previously, we had a non-negative lower bound (i.e., it was set in the
        # case above). Now we're setting this with a negative one, but there are
        # still some SOC constraints, so we cache `value` and set the variable
        # lower bound to `0.0`.
        @assert value < 0.0
        _copt_set_col_lower(model, Cint(info.column - 1), 0.0)
        info.lower_bound_if_soc = value
    else
        # Previously, we had a negative lower bound. We're setting this with
        # another negative one, but there are still some SOC constraints.
        @assert info.lower_bound_if_soc < 0.0
        info.lower_bound_if_soc = value
    end
end

"""
    _get_variable_lower_bound(model, info)

Get the current variable lower bound, ignoring a potential bound of `0.0` set
by a second order cone constraint.

See also `_set_variable_lower_bound`.
"""
function _get_variable_lower_bound(model, info)
    if !isnan(info.lower_bound_if_soc)
        # There is a value stored. That means that we must have set a value that
        # was < 0.
        @assert info.lower_bound_if_soc < 0.0
        return info.lower_bound_if_soc
    end
    return _copt_get_col_lower(model, Cint(info.column - 1))
end

function _set_variable_upper_bound(model, info, value)
    _copt_set_col_upper(model, Cint(info.column - 1), value)
    return
end

function _get_variable_upper_bound(model, info)
    return _copt_get_col_upper(model, Cint(info.column - 1))
end

function MOI.delete(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.VariableIndex,MOI.GreaterThan{Float64}},
)
    MOI.throw_if_not_valid(model, c)
    info = _info(model, c)
    _set_variable_lower_bound(model, info, -Inf)
    if info.bound == _LESS_AND_GREATER_THAN
        info.bound = _LESS_THAN
    else
        info.bound = _NONE
    end
    model.name_to_constraint_index = nothing
    return
end

function MOI.delete(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.VariableIndex,MOI.Interval{Float64}},
)
    MOI.throw_if_not_valid(model, c)
    info = _info(model, c)
    _set_variable_lower_bound(model, info, -Inf)
    _set_variable_upper_bound(model, info, Inf)
    info.bound = _NONE
    model.name_to_constraint_index = nothing
    return
end

function MOI.delete(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.VariableIndex,MOI.EqualTo{Float64}},
)
    MOI.throw_if_not_valid(model, c)
    info = _info(model, c)
    _set_variable_lower_bound(model, info, -Inf)
    _set_variable_upper_bound(model, info, Inf)
    info.bound = _NONE
    model.name_to_constraint_index = nothing
    return
end

function MOI.get(
    model::Optimizer,
    ::MOI.ConstraintSet,
    c::MOI.ConstraintIndex{MOI.VariableIndex,MOI.GreaterThan{Float64}},
)
    MOI.throw_if_not_valid(model, c)
    lower = _get_variable_lower_bound(model, _info(model, c))
    return MOI.GreaterThan(lower)
end

function MOI.get(
    model::Optimizer,
    ::MOI.ConstraintSet,
    c::MOI.ConstraintIndex{MOI.VariableIndex,MOI.LessThan{Float64}},
)
    MOI.throw_if_not_valid(model, c)
    upper = _get_variable_upper_bound(model, _info(model, c))
    return MOI.LessThan(upper)
end

function MOI.get(
    model::Optimizer,
    ::MOI.ConstraintSet,
    c::MOI.ConstraintIndex{MOI.VariableIndex,MOI.EqualTo{Float64}},
)
    MOI.throw_if_not_valid(model, c)
    lower = _get_variable_lower_bound(model, _info(model, c))
    return MOI.EqualTo(lower)
end

function MOI.get(
    model::Optimizer,
    ::MOI.ConstraintSet,
    c::MOI.ConstraintIndex{MOI.VariableIndex,MOI.Interval{Float64}},
)
    MOI.throw_if_not_valid(model, c)
    info = _info(model, c)
    lower = _get_variable_lower_bound(model, info)
    upper = _get_variable_upper_bound(model, info)
    return MOI.Interval(lower, upper)
end

function MOI.set(
    model::Optimizer,
    ::MOI.ConstraintSet,
    c::MOI.ConstraintIndex{MOI.VariableIndex,S},
    s::S,
) where {S<:_SCALAR_SETS}
    MOI.throw_if_not_valid(model, c)
    lower, upper = _bounds(s)
    info = _info(model, c)
    if lower !== nothing
        _set_variable_lower_bound(model, info, lower)
    end
    if upper !== nothing
        _set_variable_upper_bound(model, info, upper)
    end
    return
end

function MOI.add_constraint(
    model::Optimizer,
    f::MOI.VariableIndex,
    ::MOI.ZeroOne,
)
    info = _info(model, f)
    ret =
        COPT_SetColType(model.prob, 1, Cint[info.column-1], Cchar[COPT_BINARY])
    _check_ret(model, ret)
    info.type = COPT_BINARY
    return MOI.ConstraintIndex{MOI.VariableIndex,MOI.ZeroOne}(f.value)
end

function MOI.delete(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.VariableIndex,MOI.ZeroOne},
)
    MOI.throw_if_not_valid(model, c)
    info = _info(model, c)
    ret = COPT_SetColType(
        model.prob,
        1,
        Cint[info.column-1],
        Cchar[COPT_CONTINUOUS],
    )
    _check_ret(model, ret)
    info.type = COPT_CONTINUOUS
    model.name_to_constraint_index = nothing
    return
end

function MOI.get(
    model::Optimizer,
    ::MOI.ConstraintSet,
    c::MOI.ConstraintIndex{MOI.VariableIndex,MOI.ZeroOne},
)
    MOI.throw_if_not_valid(model, c)
    return MOI.ZeroOne()
end

function MOI.add_constraint(
    model::Optimizer,
    f::MOI.VariableIndex,
    ::MOI.Integer,
)
    info = _info(model, f)
    ret =
        COPT_SetColType(model.prob, 1, Cint[info.column-1], Cchar[COPT_INTEGER])
    _check_ret(model, ret)
    info.type = COPT_INTEGER
    return MOI.ConstraintIndex{MOI.VariableIndex,MOI.Integer}(f.value)
end

function MOI.delete(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.VariableIndex,MOI.Integer},
)
    MOI.throw_if_not_valid(model, c)
    info = _info(model, c)
    ret = COPT_SetColType(
        model.prob,
        1,
        Cint[info.column-1],
        Cchar[COPT_CONTINUOUS],
    )
    _check_ret(model, ret)
    info.type = COPT_CONTINUOUS
    model.name_to_constraint_index = nothing
    return
end

function MOI.get(
    model::Optimizer,
    ::MOI.ConstraintSet,
    c::MOI.ConstraintIndex{MOI.VariableIndex,MOI.Integer},
)
    MOI.throw_if_not_valid(model, c)
    return MOI.Integer()
end

###
### ScalarAffineFunction-in-Set
###

function _info(
    model::Optimizer,
    key::MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64},<:Any},
)
    if haskey(model.affine_constraint_info, key.value)
        return model.affine_constraint_info[key.value]
    end
    return throw(MOI.InvalidIndex(key))
end

function MOI.is_valid(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64},S},
) where {S}
    info = get(model.affine_constraint_info, c.value, nothing)
    if info === nothing
        return false
    else
        return typeof(info.set) == S
    end
end

function MOI.add_constraint(
    model::Optimizer,
    f::MOI.ScalarAffineFunction{Float64},
    s::Union{
        MOI.GreaterThan{Float64},
        MOI.LessThan{Float64},
        MOI.EqualTo{Float64},
    },
)
    if !iszero(f.constant)
        throw(
            MOI.ScalarFunctionConstantNotZero{Float64,typeof(f),typeof(s)}(
                f.constant,
            ),
        )
    end
    model.last_constraint_index += 1
    model.affine_constraint_info[model.last_constraint_index] =
        _ConstraintInfo(length(model.affine_constraint_info) + 1, s)
    indices, coefficients = _indices_and_coefficients(model, f)
    sense, rhs = _sense_and_rhs(s)
    ret = COPT_AddRows(
        model.prob,
        1,
        Cint[0],
        Cint[length(indices)],
        indices,
        coefficients,
        Cchar[sense],
        Cdouble[rhs],
        C_NULL,
        C_NULL,
    )
    _check_ret(model, ret)
    return MOI.ConstraintIndex{typeof(f),typeof(s)}(model.last_constraint_index)
end

function MOI.add_constraints(
    model::Optimizer,
    f::Vector{MOI.ScalarAffineFunction{Float64}},
    s::Vector{
        <:Union{
            MOI.GreaterThan{Float64},
            MOI.LessThan{Float64},
            MOI.EqualTo{Float64},
        },
    },
)
    if length(f) != length(s)
        error("Number of functions does not equal number of sets.")
    end
    canonicalized_functions = MOI.Utilities.canonical.(f)
    # First pass: compute number of non-zeros to allocate space.
    nnz = 0
    for fi in canonicalized_functions
        if !iszero(fi.constant)
            throw(
                MOI.ScalarFunctionConstantNotZero{Float64,eltype(f),eltype(s)}(
                    fi.constant,
                ),
            )
        end
        nnz += length(fi.terms)
    end
    # Initialize storage
    indices = Vector{MOI.ConstraintIndex{eltype(f),eltype(s)}}(undef, length(f))
    row_starts = Vector{Cint}(undef, length(f) + 1)
    row_starts[1] = 0
    columns = Vector{Cint}(undef, nnz)
    coefficients = Vector{Float64}(undef, nnz)
    senses = Vector{Cchar}(undef, length(f))
    rhss = Vector{Float64}(undef, length(f))
    # Second pass: loop through, passing views to _indices_and_coefficients.
    for (i, (fi, si)) in enumerate(zip(canonicalized_functions, s))
        senses[i], rhss[i] = _sense_and_rhs(si)
        row_starts[i+1] = row_starts[i] + length(fi.terms)
        _indices_and_coefficients(
            view(columns, (1+row_starts[i]):row_starts[i+1]),
            view(coefficients, (1+row_starts[i]):row_starts[i+1]),
            model,
            fi,
        )
        model.last_constraint_index += 1
        indices[i] = MOI.ConstraintIndex{eltype(f),eltype(s)}(
            model.last_constraint_index,
        )
        model.affine_constraint_info[model.last_constraint_index] =
            _ConstraintInfo(length(model.affine_constraint_info) + 1, si)
    end
    ret = COPT_AddRows(
        model.prob,
        length(f),
        row_starts,
        C_NULL,
        columns,
        coefficients,
        senses,
        rhss,
        C_NULL,
        C_NULL,
    )
    _check_ret(model, ret)
    return indices
end

function MOI.delete(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64},<:Any},
)
    row = _info(model, c).row
    ret = COPT_DelRows(model.prob, 1, Cint[row-1])
    _check_ret(model, ret)
    for (key, info) in model.affine_constraint_info
        if info.row > row
            info.row -= 1
        end
    end
    delete!(model.affine_constraint_info, c.value)
    model.name_to_constraint_index = nothing
    return
end

function MOI.get(
    model::Optimizer,
    ::MOI.ConstraintSet,
    c::MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64},S},
) where {S}
    row = _info(model, c).row
    rhs = if S <: MOI.LessThan{Float64}
        _copt_get_row_upper(model, Cint(row - 1))
    else
        _copt_get_row_lower(model, Cint(row - 1))
    end
    return S(rhs)
end

function MOI.set(
    model::Optimizer,
    ::MOI.ConstraintSet,
    c::MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64},S},
    s::S,
) where {S}
    row = _info(model, c).row
    rhs = MOI.constant(s)
    if S <: MOI.GreaterThan{Float64}
        _copt_set_row_lower(model, Cint(row - 1), rhs)
    elseif S <: MOI.LessThan{Float64}
        _copt_set_row_upper(model, Cint(row - 1), rhs)
    else
        @assert S <: MOI.EqualTo{Float64}
        _copt_set_row_lower(model, Cint(row - 1), rhs)
        _copt_set_row_upper(model, Cint(row - 1), rhs)
    end
end

function MOI.get(
    model::Optimizer,
    ::MOI.ConstraintFunction,
    c::MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64},S},
) where {S}
    row = Cint(_info(model, c).row - 1)
    p_reqsize = Ref{Cint}()
    ret = COPT_GetRows(
        model.prob,
        1,
        [row],
        C_NULL,
        C_NULL,
        C_NULL,
        C_NULL,
        0,
        p_reqsize,
    )
    _check_ret(model, ret)
    num_elem = p_reqsize[]
    rmatbeg = Vector{Cint}(undef, 2)
    rmatind = Vector{Cint}(undef, num_elem)
    rmatval = Vector{Cdouble}(undef, num_elem)
    ret = COPT_GetRows(
        model.prob,
        1,
        [row],
        rmatbeg,
        C_NULL,
        rmatind,
        rmatval,
        num_elem,
        C_NULL,
    )
    _check_ret(model, ret)
    terms = MOI.ScalarAffineTerm{Float64}[]
    for i in 1:rmatbeg[2]
        push!(
            terms,
            MOI.ScalarAffineTerm(
                rmatval[i],
                model.variable_info[CleverDicts.LinearIndex(
                    rmatind[i] + 1,
                )].index,
            ),
        )
    end
    return MOI.ScalarAffineFunction(terms, 0.0)
end

function MOI.get(
    model::Optimizer,
    ::MOI.ConstraintName,
    c::MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64},<:Any},
)
    return _info(model, c).name
end

function MOI.set(
    model::Optimizer,
    ::MOI.ConstraintName,
    c::MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64},<:Any},
    name::String,
)
    info = _info(model, c)
    if model.pass_names && info.name != name && isascii(name)
        ret = COPT_SetRowNames(model.prob, 1, Cint[info.row-1], [name])
        _check_ret(model, ret)
    end
    info.name = name
    model.name_to_constraint_index = nothing
    return
end

function MOI.get(model::Optimizer, ::Type{MOI.ConstraintIndex}, name::String)
    if model.name_to_constraint_index === nothing
        _rebuild_name_to_constraint_index(model)
    end
    if haskey(model.name_to_constraint_index, name)
        constr = model.name_to_constraint_index[name]
        if constr === nothing
            error("Duplicate constraint name detected: $(name)")
        end
        return constr
    end
    return nothing
end

function MOI.get(
    model::Optimizer,
    C::Type{MOI.ConstraintIndex{F,S}},
    name::String,
) where {F,S}
    index = MOI.get(model, MOI.ConstraintIndex, name)
    if typeof(index) == C
        return index::MOI.ConstraintIndex{F,S}
    end
    return nothing
end

function _rebuild_name_to_constraint_index(model::Optimizer)
    model.name_to_constraint_index =
        Dict{String,Union{Nothing,MOI.ConstraintIndex}}()
    _rebuild_name_to_constraint_index_util(
        model,
        model.affine_constraint_info,
        MOI.ScalarAffineFunction{Float64},
    )
    _rebuild_name_to_constraint_index_util(
        model,
        model.quadratic_constraint_info,
        MOI.ScalarQuadraticFunction{Float64},
    )
    _rebuild_name_to_constraint_index_util(
        model,
        model.sos_constraint_info,
        MOI.VectorOfVariables,
    )
    return
end

function _rebuild_name_to_constraint_index_util(model::Optimizer, dict, F)
    for (index, info) in dict
        if info.name == ""
            continue
        elseif haskey(model.name_to_constraint_index, info.name)
            model.name_to_constraint_index[info.name] = nothing
        else
            model.name_to_constraint_index[info.name] =
                MOI.ConstraintIndex{F,typeof(info.set)}(index)
        end
    end
    return
end

###
### ScalarQuadraticFunction-in-SCALAR_SET
###

function _info(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.ScalarQuadraticFunction{Float64},S},
) where {S}
    if haskey(model.quadratic_constraint_info, c.value)
        return model.quadratic_constraint_info[c.value]
    end
    return throw(MOI.InvalidIndex(c))
end

function MOI.add_constraint(
    model::Optimizer,
    f::MOI.ScalarQuadraticFunction{Float64},
    s::_SCALAR_SETS,
)
    if !iszero(f.constant)
        throw(
            MOI.ScalarFunctionConstantNotZero{Float64,typeof(f),typeof(s)}(
                f.constant,
            ),
        )
    end
    indices, coefficients, I, J, V = _indices_and_coefficients(model, f)
    # When COPT_AddQConstr() is called with an empty Q matrix, then COPT adds a
    # linear constraint if the linear matrix is non-empty, or nothing otherwise.
    # This makes is difficult to track row indices and to query slacks. Let's
    # not allow this for now.
    if length(V) == 0
        error("""
Adding a quadratic constraint with an empty Q matrix is not supported.
Add a linear constraint instead.
        """)
    end
    sense, rhs = _sense_and_rhs(s)
    ret = COPT_AddQConstr(
        model.prob,
        length(indices),
        indices,
        coefficients,
        length(V),
        I,
        J,
        V,
        sense,
        rhs,
        C_NULL,
    )
    _check_ret(model, ret)
    model.last_constraint_index += 1
    model.quadratic_constraint_info[model.last_constraint_index] =
        _ConstraintInfo(length(model.quadratic_constraint_info) + 1, s)
    return MOI.ConstraintIndex{MOI.ScalarQuadraticFunction{Float64},typeof(s)}(
        model.last_constraint_index,
    )
end

function MOI.is_valid(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.ScalarQuadraticFunction{Float64},S},
) where {S}
    info = get(model.quadratic_constraint_info, c.value, nothing)
    return info !== nothing && typeof(info.set) == S
end

function MOI.delete(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.ScalarQuadraticFunction{Float64},S},
) where {S}
    info = _info(model, c)
    ret = COPT_DelQConstrs(model.prob, 1, Cint[info.row-1])
    _check_ret(model, ret)
    for (key, info_2) in model.quadratic_constraint_info
        if info_2.row > info.row
            info_2.row -= 1
        end
    end
    delete!(model.quadratic_constraint_info, c.value)
    model.name_to_constraint_index = nothing
    return
end

function MOI.get(
    model::Optimizer,
    ::MOI.ConstraintSet,
    c::MOI.ConstraintIndex{MOI.ScalarQuadraticFunction{Float64},S},
) where {S}
    row = Cint(_info(model, c).row - 1)
    p_rhs = Ref{Cdouble}()
    ret = COPT_GetQConstrRhs(model.prob, 1, [row], p_rhs)
    _check_ret(model, ret)
    return S(p_rhs[])
end

function MOI.get(
    model::Optimizer,
    ::MOI.ConstraintFunction,
    c::MOI.ConstraintIndex{MOI.ScalarQuadraticFunction{Float64},S},
) where {S}
    copt_row = Cint(_info(model, c).row - 1)
    a, b, I, J, V = _copt_getqconstr(model, copt_row)
    affine_terms = MOI.ScalarAffineTerm{Float64}[]
    for (col, coef) in zip(a, b)
        push!(
            affine_terms,
            MOI.ScalarAffineTerm(
                coef,
                model.variable_info[CleverDicts.LinearIndex(col + 1)].index,
            ),
        )
    end
    quadratic_terms = MOI.ScalarQuadraticTerm{Float64}[]
    for (i, j, coef) in zip(I, J, V)
        new_coef = i == j ? 2coef : coef
        push!(
            quadratic_terms,
            MOI.ScalarQuadraticTerm(
                new_coef,
                model.variable_info[CleverDicts.LinearIndex(i + 1)].index,
                model.variable_info[CleverDicts.LinearIndex(j + 1)].index,
            ),
        )
    end
    return MOI.ScalarQuadraticFunction(quadratic_terms, affine_terms, 0.0)
end

function MOI.get(
    model::Optimizer,
    ::MOI.ConstraintName,
    c::MOI.ConstraintIndex{MOI.ScalarQuadraticFunction{Float64},S},
) where {S}
    return _info(model, c).name
end

function MOI.set(
    model::Optimizer,
    ::MOI.ConstraintName,
    c::MOI.ConstraintIndex{MOI.ScalarQuadraticFunction{Float64},S},
    name::String,
) where {S}
    info = _info(model, c)
    info.name = name
    model.name_to_constraint_index = nothing
    return
end

###
### VectorOfVariables-in-SecondOrderCone
###

function _info(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.SecondOrderCone},
)
    if haskey(model.quadratic_constraint_info, c.value)
        return model.quadratic_constraint_info[c.value]
    end
    return throw(MOI.InvalidIndex(c))
end

function MOI.add_constraint(
    model::Optimizer,
    f::MOI.VectorOfVariables,
    s::MOI.SecondOrderCone,
)
    if length(f.variables) != s.dimension
        error("Dimension of $(s) does not match number of terms in $(f)")
    end

    # SOC is the cone: t ≥ ||x||₂ ≥ 0. In quadratic form, this is
    # t² - Σᵢ xᵢ² ≥ 0 and t ≥ 0.

    # First, check the lower bound on t.

    t_info = _info(model, f.variables[1])
    lb = _get_variable_lower_bound(model, t_info)
    if isnan(t_info.lower_bound_if_soc) && lb < 0.0
        t_info.lower_bound_if_soc = lb
        _copt_set_col_lower(model, Cint(t_info.column - 1), 0.0)
    end
    t_info.num_soc_constraints += 1

    # Now add the quadratic constraint.

    I = Cint[column(model, v) - 1 for v in f.variables]
    V = fill(-1.0, length(f.variables))
    V[1] = 1.0
    ret = COPT_AddQConstr(
        model.prob,
        0,
        C_NULL,
        C_NULL,
        length(V),
        I,
        I,
        V,
        COPT_GREATER_EQUAL,
        0.0,
        C_NULL,
    )
    _check_ret(model, ret)
    model.last_constraint_index += 1
    model.quadratic_constraint_info[model.last_constraint_index] =
        _ConstraintInfo(length(model.quadratic_constraint_info) + 1, s)
    return MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.SecondOrderCone}(
        model.last_constraint_index,
    )
end

function MOI.is_valid(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.SecondOrderCone},
)
    info = get(model.quadratic_constraint_info, c.value, nothing)
    return info !== nothing && typeof(info.set) == MOI.SecondOrderCone
end

function MOI.delete(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.SecondOrderCone},
)
    f = MOI.get(model, MOI.ConstraintFunction(), c)
    info = _info(model, c)
    ret = COPT_DelQConstrs(model.prob, 1, Cint[info.row-1])
    _check_ret(model, ret)
    for (key, info_2) in model.quadratic_constraint_info
        if info_2.row > info.row
            info_2.row -= 1
        end
    end
    model.name_to_constraint_index = nothing
    delete!(model.quadratic_constraint_info, c.value)
    # Reset the lower bound on the `t` variable.
    t_info = _info(model, f.variables[1])
    t_info.num_soc_constraints -= 1
    if t_info.num_soc_constraints > 0
        # Don't do anything. There are still SOC associated with this variable.
        return
    elseif isnan(t_info.lower_bound_if_soc)
        # Don't do anything. It must have a >0 lower bound anyway.
        return
    end
    # There was a previous bound that we over-wrote, and it must have been
    # < 0 otherwise we wouldn't have needed to overwrite it.
    @assert t_info.lower_bound_if_soc < 0.0
    tmp_lower_bound = t_info.lower_bound_if_soc
    t_info.lower_bound_if_soc = NaN
    _set_variable_lower_bound(model, t_info, tmp_lower_bound)
    return
end

function MOI.get(
    model::Optimizer,
    ::MOI.ConstraintSet,
    c::MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.SecondOrderCone},
)
    return _info(model, c).set
end

function MOI.get(
    model::Optimizer,
    ::MOI.ConstraintFunction,
    c::MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.SecondOrderCone},
)
    copt_row = Cint(_info(model, c).row - 1)
    a, b, I, J, V = _copt_getqconstr(model, copt_row)
    @assert length(a) == length(b) == 0  # Check for no linear terms.
    t = nothing
    x = MOI.VariableIndex[]
    for (i, j, coef) in zip(I, J, V)
        v = model.variable_info[CleverDicts.LinearIndex(i + 1)].index
        @assert i == j  # Check for no off-diagonals.
        if coef == 1.0
            @assert t === nothing  # There should only be one `t`.
            t = v
        else
            @assert coef == -1.0  # The coefficients _must_ be -1 for `x` terms.
            push!(x, v)
        end
    end
    @assert t !== nothing  # Check that we found a `t` variable.
    return MOI.VectorOfVariables([t; x])
end

function MOI.get(
    model::Optimizer,
    ::MOI.ConstraintPrimal,
    c::MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.SecondOrderCone},
)
    f = MOI.get(model, MOI.ConstraintFunction(), c)
    return MOI.get(model, MOI.VariablePrimal(), f.variables)
end

function MOI.get(
    model::Optimizer,
    ::MOI.ConstraintName,
    c::MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.SecondOrderCone},
)
    return _info(model, c).name
end

function MOI.set(
    model::Optimizer,
    ::MOI.ConstraintName,
    c::MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.SecondOrderCone},
    name::String,
)
    info = _info(model, c)
    if !isempty(info.name) && model.name_to_constraint_index !== nothing
        delete!(model.name_to_constraint_index, info.name)
    end
    info.name = name
    if model.name_to_constraint_index === nothing || isempty(name)
        return
    end
    if haskey(model.name_to_constraint_index, name)
        model.name_to_constraint_index = nothing
    else
        model.name_to_constraint_index[c] = name
    end
    return
end

function MOI.get(
    model::Optimizer,
    ::MOI.ConstraintDual,
    c::MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.SecondOrderCone},
)
    f = MOI.get(model, MOI.ConstraintFunction(), c)
    # From CPLEX.jl:
    # qind = Cint(_info(model, c).row - 1)
    # surplus_p = Ref{Cint}()
    # CPXgetqconstrdslack(
    #     model.env,
    #     model.lp,
    #     qind,
    #     C_NULL,
    #     C_NULL,
    #     C_NULL,
    #     0,
    #     surplus_p,
    # )
    # ind = Vector{Cint}(undef, -surplus_p[])
    # val = Vector{Cdouble}(undef, -surplus_p[])
    # ret = CPXgetqconstrdslack(
    #     model.env,
    #     model.lp,
    #     qind,
    #     C_NULL,
    #     ind,
    #     val,
    #     -surplus_p[],
    #     surplus_p,
    # )
    # _check_ret(model, ret)
    error("COPT does not provide a dual solution for quadratic constraints.")
    slack = zeros(length(model.variable_info))
    for (i, v) in zip(ind, val)
        slack[i+1] += v
    end
    z = _dual_multiplier(model)
    return [z * slack[_info(model, v).column] for v in f.variables]
end

###
### VectorOfVariables-in-SOS{I|II}
###

const _SOS = Union{MOI.SOS1{Float64},MOI.SOS2{Float64}}

function _info(
    model::Optimizer,
    key::MOI.ConstraintIndex{MOI.VectorOfVariables,<:_SOS},
)
    if haskey(model.sos_constraint_info, key.value)
        return model.sos_constraint_info[key.value]
    end
    return throw(MOI.InvalidIndex(key))
end

_sos_type(::MOI.SOS1) = COPT_SOS_TYPE1
_sos_type(::MOI.SOS2) = COPT_SOS_TYPE2

function MOI.is_valid(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.VectorOfVariables,S},
) where {S}
    info = get(model.sos_constraint_info, c.value, nothing)
    if info === nothing || typeof(info.set) != S
        return false
    end
    f = MOI.get(model, MOI.ConstraintFunction(), c)
    return all(MOI.is_valid.(model, f.variables))
end

function MOI.add_constraint(model::Optimizer, f::MOI.VectorOfVariables, s::_SOS)
    columns = Cint[column(model, v) - 1 for v in f.variables]
    ret = COPT_AddSOSs(
        model.prob,
        1,
        Cint[_sos_type(s)],
        Cint[0],
        Cint[length(columns)],
        columns,
        s.weights,
    )
    _check_ret(model, ret)
    model.last_constraint_index += 1
    index = MOI.ConstraintIndex{MOI.VectorOfVariables,typeof(s)}(
        model.last_constraint_index,
    )
    model.sos_constraint_info[index.value] =
        _ConstraintInfo(length(model.sos_constraint_info) + 1, s)
    return index
end

function MOI.delete(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.VectorOfVariables,<:_SOS},
)
    row = Cint(_info(model, c).row - 1)
    ret = COPT_DelSOSs(model.prob, 1, [row])
    _check_ret(model, ret)
    for (key, info) in model.sos_constraint_info
        if info.row > row
            info.row -= 1
        end
    end
    delete!(model.sos_constraint_info, c.value)
    model.name_to_constraint_index = nothing
    return
end

function MOI.get(
    model::Optimizer,
    ::MOI.ConstraintName,
    c::MOI.ConstraintIndex{MOI.VectorOfVariables,<:Any},
)
    return _info(model, c).name
end

function MOI.set(
    model::Optimizer,
    ::MOI.ConstraintName,
    c::MOI.ConstraintIndex{MOI.VectorOfVariables,<:Any},
    name::String,
)
    info = _info(model, c)
    info.name = name
    model.name_to_constraint_index = nothing
    return
end

function MOI.get(
    model::Optimizer,
    ::MOI.ConstraintSet,
    c::MOI.ConstraintIndex{MOI.VectorOfVariables,S},
) where {S<:_SOS}
    row = Cint(_info(model, c).row - 1)
    sosind, soswt = _copt_getsos(model, row)
    return S(soswt)
end

function MOI.get(
    model::Optimizer,
    ::MOI.ConstraintFunction,
    c::MOI.ConstraintIndex{MOI.VectorOfVariables,S},
) where {S<:_SOS}
    row = Cint(_info(model, c).row - 1)
    sosind, soswt = _copt_getsos(model, row)
    return MOI.VectorOfVariables([
        model.variable_info[CleverDicts.LinearIndex(i + 1)].index for
        i in sosind
    ])
end

###
### Optimize methods.
###

function _has_discrete_variables(model::Optimizer)
    if length(model.sos_constraint_info) > 0
        return true
    end
    return any(v -> v.type != COPT_CONTINUOUS, values(model.variable_info))
end

function _optimize!(model)
    if _has_discrete_variables(model)
        if model.objective_type == _SCALAR_QUADRATIC ||
           !isempty(model.quadratic_constraint_info)
            error(
                "COPT cannot solve QP/QCP/SOCP models with discrete variables",
            )
        end
        model.ret_optimize = COPT_Solve(model.prob)
        model.solved_as_mip = true
    else
        model.ret_optimize = COPT_SolveLp(model.prob)
        model.solved_as_mip = false
    end
    _check_ret_optimize(model)
    return
end

function MOI.optimize!(model::Optimizer)
    if _has_discrete_variables(model)
        varindices = Cint[]
        values = Float64[]
        for (key, info) in model.variable_info
            if info.start !== nothing
                push!(varindices, Cint(info.column - 1))
                push!(values, info.start)
            end
        end
        if length(varindices) > 0
            ret = COPT_AddMipStart(
                model.prob,
                length(varindices),
                varindices,
                values,
            )
            _check_ret(model, ret)
        end
    end
    start_time = time()

    # Catch [CTRL+C], even when Julia is run from a script not in interactive
    # mode. If `true`, then a script would call `atexit` without throwing the
    # `InterruptException`. `false` is the default in interactive mode.
    Base.exit_on_sigint(false)
    _optimize!(model)
    if !isinteractive()
        Base.exit_on_sigint(true)
    end

    model.solve_time = time() - start_time
    # Infeasibility certificates are not supported by the COPT API yet.
    model.has_primal_certificate = false
    model.has_dual_certificate = false
    model.variable_primal = nothing
    return
end

function _throw_if_optimize_in_progress(model, attr) end

# LP status codes. Descriptions taken from COPT user guide.
const _RAW_LPSTATUS_STRINGS =
    Dict{Cint,Tuple{MOI.TerminationStatusCode,String}}([
        # Code => (TerminationStatus, RawStatusString)
        COPT_LPSTATUS_UNSTARTED => (
            MOI.OPTIMIZE_NOT_CALLED,
            "The LP optimization is not started yet.",
        ),
        COPT_LPSTATUS_OPTIMAL =>
            (MOI.OPTIMAL, "The LP problem is solved to optimality."),
        COPT_LPSTATUS_INFEASIBLE =>
            (MOI.INFEASIBLE, "The LP problem is infeasible."),
        COPT_LPSTATUS_UNBOUNDED =>
            (MOI.DUAL_INFEASIBLE, "The LP problem is unbounded."),
        COPT_LPSTATUS_NUMERICAL =>
            (MOI.NUMERICAL_ERROR, "Numerical trouble encountered."),
        COPT_LPSTATUS_TIMEOUT => (
            MOI.TIME_LIMIT,
            "The LP optimization is stopped because of time limit.",
        ),
        COPT_LPSTATUS_UNFINISHED => (
            MOI.NUMERICAL_ERROR,
            "The LP optimization is stopped but the solver cannot provide a solution because of numerical difficulties.",
        ),
        COPT_LPSTATUS_INTERRUPTED => (
            MOI.INTERRUPTED,
            "The LP optimization is stopped by user interrupt.",
        ),
    ])

# MIP status codes. Descriptions taken from COPT user guide.
const _RAW_MIPSTATUS_STRINGS =
    Dict{Cint,Tuple{MOI.TerminationStatusCode,String}}([
        # Code => (TerminationStatus, RawStatusString)
        COPT_MIPSTATUS_UNSTARTED => (
            MOI.OPTIMIZE_NOT_CALLED,
            "The MIP optimization is not started yet.",
        ),
        COPT_MIPSTATUS_OPTIMAL =>
            (MOI.OPTIMAL, "The MIP problem is solved to optimality."),
        COPT_MIPSTATUS_INFEASIBLE =>
            (MOI.INFEASIBLE, "The MIP problem is infeasible."),
        COPT_MIPSTATUS_UNBOUNDED =>
            (MOI.DUAL_INFEASIBLE, "The MIP problem is unbounded."),
        COPT_MIPSTATUS_INF_OR_UNB => (
            MOI.INFEASIBLE_OR_UNBOUNDED,
            "The MIP problem is infeasible or unbounded.",
        ),
        COPT_MIPSTATUS_NODELIMIT => (
            MOI.NODE_LIMIT,
            "The MIP optimization is stopped because of node limit.",
        ),
        COPT_MIPSTATUS_TIMEOUT => (
            MOI.TIME_LIMIT,
            "The MIP optimization is stopped because of time limit.",
        ),
        COPT_MIPSTATUS_UNFINISHED => (
            MOI.NUMERICAL_ERROR,
            "The MIP optimization is stopped but the solver cannot provide a solution because of numerical difficulties.",
        ),
        COPT_MIPSTATUS_INTERRUPTED => (
            MOI.INTERRUPTED,
            "The MIP optimization is stopped by user interrupt.",
        ),
    ])

function _raw_lpstatus(model::Union{Optimizer,ConeOptimizer})
    if haskey(_ERROR_TO_STATUS, model.ret_optimize)
        return _ERROR_TO_STATUS[model.ret_optimize]
    end
    status = _copt_get_int_attr(model, "LpStatus")
    if isa(model, ConeOptimizer)
        # ConeOptimizer solves the dualized problem. Flip infeasible and
        # unbounded status.
        if status == COPT_LPSTATUS_INFEASIBLE
            status = COPT_LPSTATUS_UNBOUNDED
        elseif status == COPT_LPSTATUS_UNBOUNDED
            status = COPT_LPSTATUS_INFEASIBLE
        end
    end
    if haskey(_RAW_LPSTATUS_STRINGS, status)
        return _RAW_LPSTATUS_STRINGS[status]
    end
    return error(
        """
LP termination status $(status) is not wrapped by COPT.jl.

Please open an issue at https://github.com/COPT-Public/COPT.jl/issues and
provide the complete text of this error message.
    """,
    )
end

function _raw_mipstatus(model::Union{Optimizer,ConeOptimizer})
    if haskey(_ERROR_TO_STATUS, model.ret_optimize)
        return _ERROR_TO_STATUS[model.ret_optimize]
    end
    status = _copt_get_int_attr(model, "MipStatus")
    if isa(model, ConeOptimizer)
        # ConeOptimizer solves the dualized problem. Flip infeasible and
        # unbounded status.
        if status == COPT_MIPSTATUS_INFEASIBLE
            status = COPT_MIPSTATUS_UNBOUNDED
        elseif status == COPT_MIPSTATUS_UNBOUNDED
            status = COPT_MIPSTATUS_INFEASIBLE
        end
    end
    if haskey(_RAW_MIPSTATUS_STRINGS, status)
        return _RAW_MIPSTATUS_STRINGS[status]
    end
    return error(
        """
MIP termination status $(status) is not wrapped by COPT.jl.

Please open an issue at https://github.com/COPT-Public/COPT.jl/issues and
provide the complete text of this error message.
    """,
    )
end

function MOI.get(model::Optimizer, attr::MOI.RawStatusString)
    _throw_if_optimize_in_progress(model, attr)
    if model.solved_as_mip
        return _raw_mipstatus(model)[2]
    else
        return _raw_lpstatus(model)[2]
    end
end

function MOI.get(model::Optimizer, attr::MOI.TerminationStatus)
    _throw_if_optimize_in_progress(model, attr)
    if model.solved_as_mip
        return _raw_mipstatus(model)[1]
    else
        return _raw_lpstatus(model)[1]
    end
end

function MOI.get(model::Optimizer, attr::MOI.PrimalStatus)
    _throw_if_optimize_in_progress(model, attr)
    if attr.result_index != 1
        return MOI.NO_SOLUTION
    end
    attr_name = model.solved_as_mip ? "HasMipSol" : "HasLpSol"
    has_sol = _copt_get_int_attr(model, attr_name)
    if has_sol != 0
        return MOI.FEASIBLE_POINT
    end
    return MOI.NO_SOLUTION
end

function MOI.get(model::Optimizer, attr::MOI.DualStatus)
    _throw_if_optimize_in_progress(model, attr)
    if attr.result_index != 1
        return MOI.NO_SOLUTION
    end
    # Dual solution only available for non-MIP solve.
    if !model.solved_as_mip
        has_sol = _copt_get_int_attr(model, "HasLpSol")
        if has_sol != 0
            return MOI.FEASIBLE_POINT
        end
    end
    return MOI.NO_SOLUTION
end

_update_cache(::Optimizer, data::Vector{Float64}) = data

function _update_cache(model::Optimizer, ::Nothing)
    n = length(model.variable_info)
    x = zeros(n)
    ret = if model.solved_as_mip
        COPT_GetSolution(model.prob, x)
    else
        COPT_GetLpSolution(model.prob, x, C_NULL, C_NULL, C_NULL)
    end
    _check_ret(model, ret)
    return x
end

function MOI.get(
    model::Optimizer,
    attr::MOI.VariablePrimal,
    x::Union{MOI.VariableIndex,Vector{MOI.VariableIndex}},
)
    _throw_if_optimize_in_progress(model, attr)
    MOI.check_result_index_bounds(model, attr)
    if model.has_primal_certificate
        return model.certificate[column(model, x)]
    end
    model.variable_primal = _update_cache(model, model.variable_primal)
    return model.variable_primal[column(model, x)]
end

function MOI.get(
    model::Optimizer,
    attr::MOI.ConstraintPrimal,
    c::MOI.ConstraintIndex{MOI.VariableIndex,<:Any},
)
    _throw_if_optimize_in_progress(model, attr)
    MOI.check_result_index_bounds(model, attr)
    return MOI.get(model, MOI.VariablePrimal(), MOI.VariableIndex(c.value))
end

function MOI.get(
    model::Optimizer,
    attr::MOI.ConstraintPrimal,
    c::MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64},<:Any},
)
    _throw_if_optimize_in_progress(model, attr)
    MOI.check_result_index_bounds(model, attr)
    row = Cint(_info(model, c).row - 1)
    ax = 0.0
    # COPT_GetRowInfo() allows to query the row value only for non-MIP solutions.
    if model.solved_as_mip
        columns, coefficients = _copt_get_row(model, row)
        model.variable_primal = _update_cache(model, model.variable_primal)
        for (col, coef) in zip(columns, coefficients)
            ax += coef * model.variable_primal[col+1]
        end
    else
        ax = _copt_get_row_info(model, "Slack", row)
    end
    return ax
end

function MOI.get(
    model::Optimizer,
    attr::MOI.ConstraintPrimal,
    c::MOI.ConstraintIndex{MOI.ScalarQuadraticFunction{Float64},<:Any},
)
    _throw_if_optimize_in_progress(model, attr)
    MOI.check_result_index_bounds(model, attr)
    row = Cint(_info(model, c).row - 1)
    p_value = Ref{Cdouble}()
    ret = COPT_GetQConstrInfo(model.prob, "Slack", 1, [row], p_value)
    _check_ret(model, ret)
    return p_value[]
end

function _dual_multiplier(model::Optimizer)
    return MOI.get(model, MOI.ObjectiveSense()) == MOI.MIN_SENSE ? 1.0 : -1.0
end

"""
    _farkas_variable_dual(model::Optimizer, col::Cint)

Return a Farkas dual associated with the variable bounds of `col`.

Compute the Farkas dual as:

    ā * x = λ' * A * x <= λ' * b = -β + sum(āᵢ * Uᵢ | āᵢ < 0) + sum(āᵢ * Lᵢ | āᵢ > 0)

The Farkas dual of the variable is ā, and it applies to the upper bound if ā < 0,
and it applies to the lower bound if ā > 0.
"""
function _farkas_variable_dual(model::Optimizer, col::Cint)
    p_reqsize = Ref{Cint}()
    ret = COPT_GetCols(
        model.prob,
        1,
        [col],
        C_NULL,
        C_NULL,
        C_NULL,
        C_NULL,
        0,
        p_reqsize,
    )
    _check_ret(model, ret)
    num_elem = p_reqsize[]
    cmatbeg = Vector{Cint}(undef, 2)
    cmatind = Vector{Cint}(undef, num_elem)
    cmatval = Vector{Cdouble}(undef, num_elem)
    ret = COPT_GetCols(
        model.prob,
        1,
        [col],
        cmatbeg,
        C_NULL,
        cmatind,
        cmatval,
        num_elem,
        C_NULL,
    )
    _check_ret(model, ret)
    return sum(v * model.certificate[i+1] for (i, v) in zip(cmatind, cmatval))
end

function MOI.get(
    model::Optimizer,
    attr::MOI.ConstraintDual,
    c::MOI.ConstraintIndex{MOI.VariableIndex,MOI.LessThan{Float64}},
)
    _throw_if_optimize_in_progress(model, attr)
    MOI.check_result_index_bounds(model, attr)
    col = Cint(column(model, c) - 1)
    if model.has_dual_certificate
        dual = -_farkas_variable_dual(model, col)
        return min(0.0, dual)
    end
    redcost = _copt_get_col_info(model, "RedCost", col)
    sense = MOI.get(model, MOI.ObjectiveSense())
    # The following is a heuristic for determining whether the reduced cost
    # applies to the lower or upper bound. It can be wrong by at most
    # `FeasibilityTol`.
    if sense == MOI.MIN_SENSE && redcost < 0
        # If minimizing, the reduced cost must be negative (ignoring
        # tolerances).
        return redcost
    elseif sense == MOI.MAX_SENSE && redcost > 0
        # If minimizing, the reduced cost must be positive (ignoring
        # tolerances). However, because of the MOI dual convention, we return a
        # negative value.
        return -redcost
    else
        # The reduced cost, if non-zero, must related to the lower bound.
        return 0.0
    end
end

function MOI.get(
    model::Optimizer,
    attr::MOI.ConstraintDual,
    c::MOI.ConstraintIndex{MOI.VariableIndex,MOI.GreaterThan{Float64}},
)
    _throw_if_optimize_in_progress(model, attr)
    MOI.check_result_index_bounds(model, attr)
    col = Cint(column(model, c) - 1)
    if model.has_dual_certificate
        dual = -_farkas_variable_dual(model, col)
        return max(0.0, dual)
    end
    redcost = _copt_get_col_info(model, "RedCost", col)
    sense = MOI.get(model, MOI.ObjectiveSense())
    # The following is a heuristic for determining whether the reduced cost
    # applies to the lower or upper bound. It can be wrong by at most
    # `FeasibilityTol`.
    if sense == MOI.MIN_SENSE && redcost > 0
        # If minimizing, the reduced cost must be negative (ignoring
        # tolerances).
        return redcost
    elseif sense == MOI.MAX_SENSE && redcost < 0
        # If minimizing, the reduced cost must be positive (ignoring
        # tolerances). However, because of the MOI dual convention, we return a
        # negative value.
        return -redcost
    else
        # The reduced cost, if non-zero, must related to the lower bound.
        return 0.0
    end
end

function MOI.get(
    model::Optimizer,
    attr::MOI.ConstraintDual,
    c::MOI.ConstraintIndex{
        MOI.VariableIndex,
        <:Union{MOI.Interval{Float64},MOI.EqualTo{Float64}},
    },
)
    _throw_if_optimize_in_progress(model, attr)
    MOI.check_result_index_bounds(model, attr)
    col = Cint(column(model, c) - 1)
    if model.has_dual_certificate
        return -_farkas_variable_dual(model, col)
    end
    redcost = _copt_get_col_info(model, "RedCost", col)
    return _dual_multiplier(model) * redcost
end

function MOI.get(
    model::Optimizer,
    attr::MOI.ConstraintDual,
    c::MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64},<:Any},
)
    _throw_if_optimize_in_progress(model, attr)
    MOI.check_result_index_bounds(model, attr)
    row = Cint(_info(model, c).row - 1)
    if model.has_dual_certificate
        return model.certificate[row+1]
    end
    rowdual = _copt_get_row_info(model, "Dual", row)
    return _dual_multiplier(model) * rowdual
end

function MOI.get(
    model::Optimizer,
    attr::MOI.ConstraintDual,
    c::MOI.ConstraintIndex{MOI.ScalarQuadraticFunction{Float64},<:Any},
)
    # For more information on QCP duals, see
    # https://www.ibm.com/support/knowledgecenter/SSSA5P_12.10.0/ilog.odms.cplex.help/CPLEX/UsrMan/topics/cont_optim/qcp/17_QCP_duals.html
    _throw_if_optimize_in_progress(model, attr)
    MOI.check_result_index_bounds(model, attr)
    if model.has_dual_certificate
        error("Infeasibility certificate not available for $(c)")
    end
    # The derivative of a quadratic f(x) = x^TQx + a^Tx + b <= 0 is
    # ∇f(x) = Q^Tx + Qx + a
    # The dual is undefined if x is at the point of the cone. This can only be
    # checked to numeric tolerances. We use `cone_top_tol`.
    cone_top, cone_top_tol = true, 1e-6
    x = zeros(length(model.variable_info))
    ret = COPT_GetLpSolution(model.prob, x, C_NULL, C_NULL, C_NULL)
    _check_ret(model, ret)
    ∇f = zeros(length(x))
    copt_row = Cint(_info(model, c).row - 1)
    a_i, a_v, qrow, qcol, qval = _copt_getqconstr(model, copt_row)
    for (i, j, v) in zip(qrow, qcol, qval)
        ∇f[i+1] += v * x[j+1]
        ∇f[j+1] += v * x[i+1]
        if abs(x[i+1]) > cone_top_tol || abs(x[j+1]) > cone_top_tol
            cone_top = false
        end
    end
    for (i, v) in zip(a_i, a_v)
        ∇f[i+1] += v
        if abs(x[i+1]) > cone_top_tol
            cone_top = false
        end
    end
    # TODO(odow): if at top of cone (x = 0) dual multiplier is ill-formed.
    if cone_top
        return NaN
    end
    qind = Cint(_info(model, c).row - 1)
    # From CPLEX.jl:
    # nz_p, surplus_p = Ref{Cint}(), Ref{Cint}()
    # CPXgetqconstrdslack(
    #     model.env,
    #     model.lp,
    #     qind,
    #     nz_p,
    #     C_NULL,
    #     C_NULL,
    #     0,
    #     surplus_p,
    # )
    # ind = Vector{Cint}(undef, -surplus_p[])
    # val = Vector{Cdouble}(undef, -surplus_p[])
    # ret = CPXgetqconstrdslack(
    #     model.env,
    #     model.lp,
    #     qind,
    #     nz_p,
    #     ind,
    #     val,
    #     -surplus_p[],
    #     surplus_p,
    # )
    # _check_ret(model, ret)
    error("COPT does not provide a dual solution for quadratic constraints.")
    ∇f_max, ∇f_i = findmax(abs.(∇f))
    if ∇f_max > cone_top_tol
        for (i, v) in zip(ind, val)
            if i + 1 == ∇f_i
                return _dual_multiplier(model) * v / ∇f[∇f_i]
            end
        end
    end
    return 0.0
end

function MOI.get(model::Optimizer, attr::MOI.ObjectiveValue)
    _throw_if_optimize_in_progress(model, attr)
    MOI.check_result_index_bounds(model, attr)
    return _copt_get_dbl_attr(
        model,
        model.solved_as_mip ? "BestObj" : "LpObjval",
    )
end

function MOI.get(model::Optimizer, attr::MOI.ObjectiveBound)
    _throw_if_optimize_in_progress(model, attr)
    return _copt_get_dbl_attr(
        model,
        model.solved_as_mip ? "BestBnd" : "LpObjval",
    )
end

function MOI.get(model::Union{Optimizer,ConeOptimizer}, attr::MOI.SolveTimeSec)
    _throw_if_optimize_in_progress(model, attr)
    return model.solve_time
end

function MOI.get(model::Optimizer, attr::MOI.SimplexIterations)
    _throw_if_optimize_in_progress(model, attr)
    return convert(Int64, _copt_get_int_attr(model, "SimplexIter"))
end

function MOI.get(model::Optimizer, attr::MOI.BarrierIterations)
    _throw_if_optimize_in_progress(model, attr)
    return convert(Int64, _copt_get_int_attr(model, "BarrierIter"))
end

function MOI.get(model::Optimizer, attr::MOI.NodeCount)
    _throw_if_optimize_in_progress(model, attr)
    return convert(Int64, _copt_get_int_attr(model, "NodeCnt"))
end

function MOI.get(model::Optimizer, attr::MOI.RelativeGap)
    _throw_if_optimize_in_progress(model, attr)
    return _copt_get_dbl_attr(model, "BestGap")
end

function MOI.get(model::Optimizer, attr::MOI.DualObjectiveValue)
    _throw_if_optimize_in_progress(model, attr)
    MOI.check_result_index_bounds(model, attr)
    return _copt_get_dbl_attr(
        model,
        model.solved_as_mip ? "BestBnd" : "LpObjval",
    )
end

function MOI.get(model::Optimizer, attr::MOI.ResultCount)
    _throw_if_optimize_in_progress(model, attr)
    if model.has_dual_certificate
        return 1
    elseif model.has_primal_certificate
        return 1
    elseif model.solved_as_mip
        return _copt_get_int_attr(model, "HasMipSol") == 0 ? 0 : 1
    else
        return _copt_get_int_attr(model, "HasLpSol") == 0 ? 0 : 1
    end
end

function MOI.get(model::Union{Optimizer,ConeOptimizer}, ::MOI.Silent)
    return model.silent
end

function MOI.set(
    model::Union{Optimizer,ConeOptimizer},
    ::MOI.Silent,
    flag::Bool,
)
    model.silent = flag
    MOI.set(model, MOI.RawOptimizerAttribute("LogToConsole"), flag ? 0 : 1)
    return
end

function MOI.get(model::Union{Optimizer,ConeOptimizer}, ::MOI.NumberOfThreads)
    return Int(MOI.get(model, MOI.RawOptimizerAttribute("Threads")))
end

function MOI.set(
    model::Union{Optimizer,ConeOptimizer},
    ::MOI.NumberOfThreads,
    x::Int,
)
    return MOI.set(model, MOI.RawOptimizerAttribute("Threads"), x)
end

MOI.get(model::Union{Optimizer,ConeOptimizer}, ::MOI.Name) = model.name

function MOI.set(
    model::Union{Optimizer,ConeOptimizer},
    ::MOI.Name,
    name::String,
)
    return model.name = name
end

MOI.get(model::Optimizer, ::MOI.NumberOfVariables) = length(model.variable_info)
function MOI.get(model::Optimizer, ::MOI.ListOfVariableIndices)
    return sort!(collect(keys(model.variable_info)), by = x -> x.value)
end

MOI.get(model::Optimizer, ::MOI.RawSolver) = model.prob

function MOI.set(
    model::Optimizer,
    ::MOI.VariablePrimalStart,
    x::MOI.VariableIndex,
    value::Union{Nothing,Float64},
)
    info = _info(model, x)
    info.start = value
    return
end

function MOI.get(
    model::Optimizer,
    ::MOI.VariablePrimalStart,
    x::MOI.VariableIndex,
)
    return _info(model, x).start
end

function MOI.supports(
    ::Optimizer,
    ::MOI.VariablePrimalStart,
    ::Type{MOI.VariableIndex},
)
    return true
end

function MOI.get(model::Optimizer, ::MOI.NumberOfConstraints{F,S}) where {F,S}
    # TODO: this could be more efficient.
    return length(MOI.get(model, MOI.ListOfConstraintIndices{F,S}()))
end

_bound_enums(::Type{<:MOI.LessThan}) = (_LESS_THAN, _LESS_AND_GREATER_THAN)
function _bound_enums(::Type{<:MOI.GreaterThan})
    return (_GREATER_THAN, _LESS_AND_GREATER_THAN)
end
_bound_enums(::Type{<:MOI.Interval}) = (_INTERVAL,)
_bound_enums(::Type{<:MOI.EqualTo}) = (_EQUAL_TO,)
_bound_enums(::Any) = (nothing,)

_type_enums(::Type{MOI.ZeroOne}) = (COPT_BINARY,)
_type_enums(::Type{MOI.Integer}) = (COPT_INTEGER,)
_type_enums(::Any) = (nothing,)

function MOI.get(
    model::Optimizer,
    ::MOI.ListOfConstraintIndices{MOI.VariableIndex,S},
) where {S}
    indices = MOI.ConstraintIndex{MOI.VariableIndex,S}[]
    for (key, info) in model.variable_info
        if info.bound in _bound_enums(S) || info.type in _type_enums(S)
            push!(indices, MOI.ConstraintIndex{MOI.VariableIndex,S}(key.value))
        end
    end
    return sort!(indices, by = x -> x.value)
end

function MOI.get(
    model::Optimizer,
    ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Float64},S},
) where {S}
    indices = MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64},S}[]
    for (key, info) in model.affine_constraint_info
        if typeof(info.set) == S
            push!(
                indices,
                MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64},S}(key),
            )
        end
    end
    return sort!(indices, by = x -> x.value)
end

function MOI.get(
    model::Optimizer,
    ::MOI.ListOfConstraintIndices{MOI.ScalarQuadraticFunction{Float64},S},
) where {S}
    indices = MOI.ConstraintIndex{MOI.ScalarQuadraticFunction{Float64},S}[]
    for (key, info) in model.quadratic_constraint_info
        if typeof(info.set) == S
            push!(
                indices,
                MOI.ConstraintIndex{MOI.ScalarQuadraticFunction{Float64},S}(
                    key,
                ),
            )
        end
    end
    return sort!(indices, by = x -> x.value)
end

function MOI.get(
    model::Optimizer,
    ::MOI.ListOfConstraintIndices{MOI.VectorOfVariables,S},
) where {S<:Union{<:MOI.SOS1,<:MOI.SOS2}}
    indices = MOI.ConstraintIndex{MOI.VectorOfVariables,S}[]
    for (key, info) in model.sos_constraint_info
        if typeof(info.set) == S
            push!(indices, MOI.ConstraintIndex{MOI.VectorOfVariables,S}(key))
        end
    end
    return sort!(indices, by = x -> x.value)
end

function MOI.get(
    model::Optimizer,
    ::MOI.ListOfConstraintIndices{MOI.VectorOfVariables,MOI.SecondOrderCone},
)
    indices = MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.SecondOrderCone}[
        MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.SecondOrderCone}(key)
        for (key, info) in model.quadratic_constraint_info if
        typeof(info.set) == MOI.SecondOrderCone
    ]
    return sort!(indices, by = x -> x.value)
end

function MOI.get(model::Optimizer, ::MOI.ListOfConstraintTypesPresent)
    constraints = Set{Tuple{DataType,DataType}}()
    for info in values(model.variable_info)
        if info.bound == _NONE
        elseif info.bound == _LESS_THAN
            push!(constraints, (MOI.VariableIndex, MOI.LessThan{Float64}))
        elseif info.bound == _GREATER_THAN
            push!(constraints, (MOI.VariableIndex, MOI.GreaterThan{Float64}))
        elseif info.bound == _LESS_AND_GREATER_THAN
            push!(constraints, (MOI.VariableIndex, MOI.LessThan{Float64}))
            push!(constraints, (MOI.VariableIndex, MOI.GreaterThan{Float64}))
        elseif info.bound == _EQUAL_TO
            push!(constraints, (MOI.VariableIndex, MOI.EqualTo{Float64}))
        elseif info.bound == _INTERVAL
            push!(constraints, (MOI.VariableIndex, MOI.Interval{Float64}))
        end
        if info.type == COPT_CONTINUOUS
        elseif info.type == COPT_BINARY
            push!(constraints, (MOI.VariableIndex, MOI.ZeroOne))
        elseif info.type == COPT_INTEGER
            push!(constraints, (MOI.VariableIndex, MOI.Integer))
        end
    end
    for info in values(model.affine_constraint_info)
        push!(
            constraints,
            (MOI.ScalarAffineFunction{Float64}, typeof(info.set)),
        )
    end
    for info in values(model.quadratic_constraint_info)
        if typeof(info.set) == MOI.SecondOrderCone
            push!(constraints, (MOI.VectorOfVariables, MOI.SecondOrderCone))
        else
            push!(
                constraints,
                (MOI.ScalarQuadraticFunction{Float64}, typeof(info.set)),
            )
        end
    end
    for info in values(model.sos_constraint_info)
        push!(constraints, (MOI.VectorOfVariables, typeof(info.set)))
    end
    return collect(constraints)
end

function MOI.get(model::Optimizer, ::MOI.ObjectiveFunctionType)
    if model.objective_type == _SINGLE_VARIABLE
        return MOI.VariableIndex
    elseif model.objective_type == _SCALAR_AFFINE
        return MOI.ScalarAffineFunction{Float64}
    else
        @assert model.objective_type == _SCALAR_QUADRATIC
        return MOI.ScalarQuadraticFunction{Float64}
    end
end

function MOI.modify(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64},<:Any},
    chg::MOI.ScalarCoefficientChange{Float64},
)
    col = Cint(column(model, chg.variable) - 1)
    row = Cint(_info(model, c).row - 1)
    ret = COPT_SetElem(model.prob, col, row, chg.new_coefficient)
    _check_ret(model, ret)
    return
end

function MOI.modify(
    model::Optimizer,
    ::MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}},
    chg::MOI.ScalarCoefficientChange{Float64},
)
    col = Cint(column(model, chg.variable) - 1)
    ret = COPT_SetColObj(model.prob, 1, [col], [chg.new_coefficient])
    _check_ret(model, ret)
    if model.objective_type == _UNSET_OBJECTIVE ||
       model.objective_type == _SINGLE_VARIABLE
        model.objective_type = _SCALAR_AFFINE
    end
    return
end

"""
    _replace_with_matching_sparsity!(
        model::Optimizer,
        previous::MOI.ScalarAffineFunction,
        replacement::MOI.ScalarAffineFunction, row::Int
    )

Internal function, not intended for external use.

Change the linear constraint function at index `row` in `model` from
`previous` to `replacement`. This function assumes that `previous` and
`replacement` have exactly the same sparsity pattern w.r.t. which variables
they include and that both constraint functions are in canonical form (as
returned by `MOIU.canonical()`. Neither assumption is checked within the body
of this function.
"""
function _replace_with_matching_sparsity!(
    model::Optimizer,
    previous::MOI.ScalarAffineFunction,
    replacement::MOI.ScalarAffineFunction,
    row::Int,
)
    for term in replacement.terms
        col = Cint(column(model, term.variable) - 1)
        ret =
            COPT_SetElem(model.prob, col, Cint(row - 1), MOI.coefficient(term))
        _check_ret(model, ret)
    end
    return
end

"""
    _replace_with_different_sparsity!(
        model::Optimizer,
        previous::MOI.ScalarAffineFunction,
        replacement::MOI.ScalarAffineFunction, row::Int
    )

Internal function, not intended for external use.

    Change the linear constraint function at index `row` in `model` from
`previous` to `replacement`. This function assumes that `previous` and
`replacement` may have different sparsity patterns.

This function (and `_replace_with_matching_sparsity!` above) are necessary
because in order to fully replace a linear constraint, we have to zero out the
current matrix coefficients and then set the new matrix coefficients. When the
sparsity patterns match, the zeroing-out step can be skipped.
"""
function _replace_with_different_sparsity!(
    model::Optimizer,
    previous::MOI.ScalarAffineFunction,
    replacement::MOI.ScalarAffineFunction,
    row::Int,
)
    # First, zero out the old constraint function terms.
    for term in previous.terms
        col = Cint(column(model, term.variable) - 1)
        ret = COPT_SetElem(model.prob, col, Cint(row - 1), 0.0)
        _check_ret(model, ret)
    end

    # Next, set the new constraint function terms.
    for term in previous.terms
        col = Cint(column(model, term.variable) - 1)
        ret =
            COPT_SetElem(model.prob, col, Cint(row - 1), MOI.coefficient(term))
        _check_ret(model, ret)
    end
    return
end

"""
    _matching_sparsity_pattern(
        f1::MOI.ScalarAffineFunction{Float64},
        f2::MOI.ScalarAffineFunction{Float64}
    )

Internal function, not intended for external use.

Determines whether functions `f1` and `f2` have the same sparsity pattern
w.r.t. their constraint columns. Assumes both functions are already in
canonical form.
"""
function _matching_sparsity_pattern(
    f1::MOI.ScalarAffineFunction{Float64},
    f2::MOI.ScalarAffineFunction{Float64},
)
    if axes(f1.terms) != axes(f2.terms)
        return false
    end
    for (f1_term, f2_term) in zip(f1.terms, f2.terms)
        if MOI.term_indices(f1_term) != MOI.term_indices(f2_term)
            return false
        end
    end
    return true
end

function MOI.set(
    model::Optimizer,
    ::MOI.ConstraintFunction,
    c::MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64},S},
    f::MOI.ScalarAffineFunction{Float64},
) where {S<:_SCALAR_SETS}
    previous = MOI.get(model, MOI.ConstraintFunction(), c)
    MOI.Utilities.canonicalize!(previous)
    replacement = MOI.Utilities.canonical(f)
    # If the previous and replacement constraint functions have exactly
    # the same sparsity pattern, then we can take a faster path by just
    # passing the replacement terms to the model. But if their sparsity
    # patterns differ, then we need to first zero out the previous terms
    # and then set the replacement terms.
    row = _info(model, c).row
    if _matching_sparsity_pattern(previous, replacement)
        _replace_with_matching_sparsity!(model, previous, replacement, row)
    else
        _replace_with_different_sparsity!(model, previous, replacement, row)
    end
    # Change right-hand side.
    if S <: MOI.GreaterThan{Float64} ||
       S <: MOI.EqualTo{Float64} ||
       S <: MOI.Interval{Float64}
        lower = _copt_get_row_lower(model, Cint(row - 1))
        @assert isfinite(lower)
        lower -= replacement.constant - previous.constant
        _copt_set_row_lower(model, Cint(row - 1), lower)
    end
    if S <: MOI.LessThan{Float64} ||
       S <: MOI.EqualTo{Float64} ||
       S <: MOI.Interval{Float64}
        upper = _copt_get_row_upper(model, Cint(row - 1))
        @assert isfinite(upper)
        upper -= replacement.constant - previous.constant
        _copt_set_row_upper(model, Cint(row - 1), upper)
    end
    return
end

function MOI.get(
    model::Optimizer,
    ::MOI.ConstraintBasisStatus,
    c::MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64},S},
) where {S<:_SCALAR_SETS}
    rstat = Vector{Cint}(undef, length(model.affine_constraint_info))
    ret = COPT_GetBasis(model.prob, C_NULL, rstat)
    _check_ret(model, ret)
    cbasis = rstat[_info(model, c).row]
    if cbasis == COPT_BASIS_BASIC
        return MOI.BASIC
    else
        return MOI.NONBASIC
    end
end

function MOI.get(
    model::Optimizer,
    ::MOI.VariableBasisStatus,
    x::MOI.VariableIndex,
)
    cstat = Vector{Cint}(undef, length(model.variable_info))
    ret = COPT_GetBasis(model.prob, cstat, C_NULL)
    _check_ret(model, ret)
    vbasis = cstat[_info(model, x).column]
    if vbasis == COPT_BASIS_BASIC
        return MOI.BASIC
    elseif vbasis == COPT_BASIS_SUPERBASIC
        return MOI.SUPER_BASIC
    elseif vbasis == COPT_BASIS_LOWER
        return MOI.NONBASIC_AT_LOWER
    elseif vbasis == COPT_BASIS_UPPER
        return MOI.NONBASIC_AT_UPPER
    else
        @assert vbasis == COPT_BASIS_FIXED
        return MOI.NONBASIC
    end
end

###
### Optimize methods for ConeOptimizer.
###

function _map_sets(f, sets, ::Type{S}) where {S}
    F = MOI.VectorAffineFunction{Float64}
    cis = MOI.get(sets, MOI.ListOfConstraintIndices{F,S}())
    return Int[f(MOI.get(sets, MOI.ConstraintSet(), ci)) for ci in cis]
end

function MOI.optimize!(dest::ConeOptimizer, src::OptimizerCache)
    MOI.empty!(dest)
    index_map = MOI.Utilities.identity_index_map(src)
    Ac = src.constraints
    A = Ac.coefficients

    model_attributes = MOI.get(src, MOI.ListOfModelAttributesSet())
    objective_function_attr =
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}()
    b = zeros(A.n)
    max_sense = MOI.get(src, MOI.ObjectiveSense()) == MOI.MAX_SENSE
    objective_constant = 0.0
    if objective_function_attr in MOI.get(src, MOI.ListOfModelAttributesSet())
        obj = MOI.get(src, objective_function_attr)
        objective_constant = MOI.constant(obj)
        for term in obj.terms
            b[term.variable.value] += (max_sense ? 1 : -1) * term.coefficient
        end
    end

    A = SparseMatrixCSC(A.m, A.n, A.colptr, A.rowval, -A.nzval)

    nFree = Ac.sets.num_rows[1]
    nPositive = Ac.sets.num_rows[2] - Ac.sets.num_rows[1]
    socDim = _map_sets(MOI.dimension, Ac, MOI.SecondOrderCone)
    rotDim = _map_sets(MOI.dimension, Ac, MOI.RotatedSecondOrderCone)
    psdDim = _map_sets(MOI.side_dimension, Ac, ReorderedPSDCone)

    c = Ac.constants
    dest.cones = deepcopy(Ac.sets)

    A_copt = sparse(A')
    nRow, nCol = size(A_copt)
    nBox = 0
    nCone = length(socDim)
    nRotatedCone = length(rotDim)
    nPrimalExp = 0
    nDualExp = 0
    nPrimalPow = 0
    nDualPow = 0
    nPSD = length(psdDim)
    nQObjElem = 0
    colObj = -c
    colMatBeg = Cint.(A_copt.colptr[1:nCol] .- 1)
    colMatCnt = Cint.(diff(A_copt.colptr))
    colMatIdx = Cint.(A_copt.rowval .- 1)
    colMatElem = A_copt.nzval
    rowRhs = b
    outRowMap = zeros(Cint, nRow)

    # Use undocumented COPT function
    # int COPT_CALL COPT_LoadConeProb(copt_prob* prob,
    #  int nCol,
    #  int nRow,
    #  int nFree,
    #  int nPositive,
    #  int nBox,
    #  int nCone,
    #  int nRotateCone,
    #  int nPrimalExp,
    #  int nDualExp,
    #  int nPrimalPow,
    #  int nDualPow,
    #  int nPSD,
    #  int nQObjElem,
    #  int iObjSense,
    #  double dObjConst,
    #  const double* colObj,
    #  const int* qObjRow,
    #  const int* qObjCol,
    #  const double* qObjElem,
    #  const int* colMatBeg,
    #  const int* colMatCnt,
    #  const int* colMatIdx,
    #  const double* colMatElem,
    #  const double* rowRhs,
    #  const double* boxLower,
    #  const double* boxUpper,
    #  const int* coneDim,
    #  const int* rotateConeDim,
    #  const int* primalPowDim,
    #  const int* dualPowDim,
    #  const double* primalPowAlpha,
    #  const double* dualPowAlpha,
    #  const int* psdDim,
    #  const char* colType,
    #  char const* const* colNames,
    #  char const* const* rowNames,
    #  char const* const* psdColNames,
    #  int* outRowMap)
    ret = ccall(
        (:COPT_LoadConeProb, libcopt),
        Cint,
        (
            Ptr{copt_prob},
            Cint,
            Cint,
            Cint,
            Cint,
            Cint,
            Cint,
            Cint,
            Cint,
            Cint,
            Cint,
            Cint,
            Cint,
            Cint,
            Cint,
            Cdouble,
            Ptr{Cdouble},
            Ptr{Cint},
            Ptr{Cint},
            Ptr{Cdouble},
            Ptr{Cint},
            Ptr{Cint},
            Ptr{Cint},
            Ptr{Cdouble},
            Ptr{Cdouble},
            Ptr{Cdouble},
            Ptr{Cdouble},
            Ptr{Cint},
            Ptr{Cint},
            Ptr{Cint},
            Ptr{Cint},
            Ptr{Cdouble},
            Ptr{Cdouble},
            Ptr{Cint},
            Ptr{Cchar},
            Ptr{Ptr{Cchar}},
            Ptr{Ptr{Cchar}},
            Ptr{Ptr{Cchar}},
            Ptr{Cint},
        ),
        dest.prob,
        nCol,
        nRow,
        nFree,
        nPositive,
        nBox,
        nCone,
        nRotatedCone,
        nPrimalExp,
        nDualExp,
        nPrimalPow,
        nDualPow,
        nPSD,
        nQObjElem,
        COPT_MAXIMIZE,
        objective_constant,
        colObj,
        C_NULL,
        C_NULL,
        C_NULL,
        colMatBeg,
        colMatCnt,
        colMatIdx,
        colMatElem,
        rowRhs,
        C_NULL,
        C_NULL,
        convert(Array{Cint}, socDim),
        convert(Array{Cint}, rotDim),
        C_NULL,
        C_NULL,
        C_NULL,
        C_NULL,
        convert(Array{Cint}, psdDim),
        C_NULL,
        C_NULL,
        C_NULL,
        C_NULL,
        outRowMap,
    )
    _check_ret(dest, ret)

    nScalarCol = _copt_get_int_attr(dest, "Cols")
    nPSDColLen = _copt_get_int_attr(dest, "PSDLens")
    nScalarRow = _copt_get_int_attr(dest, "Rows")
    nPSDRow = _copt_get_int_attr(dest, "PSDConstrs")

    # Catch [CTRL+C], even when Julia is run from a script not in interactive
    # mode. If `true`, then a script would call `atexit` without throwing the
    # `InterruptException`. `false` is the default in interactive mode.
    Base.exit_on_sigint(false)
    start_time = time()
    dest.ret_optimize = COPT_Solve(dest.prob)
    dest.solve_time = time() - start_time
    if !isinteractive()
        Base.exit_on_sigint(true)
    end
    _check_ret_optimize(dest)

    lpStatus = _copt_get_int_attr(dest, "LpStatus")
    if (lpStatus == COPT_LPSTATUS_OPTIMAL)
        scalarColVal = zeros(Cdouble, nScalarCol)
        psdColVal = zeros(Cdouble, nPSDColLen)
        scalarRowDual = zeros(Cdouble, nScalarRow)
        psdRowDual = zeros(Cdouble, nPSDRow)

        primalSol = zeros(Cdouble, nCol)
        dualSol = zeros(Cdouble, nRow)

        # Get LP solution
        ret = COPT_GetLpSolution(
            dest.prob,
            scalarColVal,
            C_NULL,
            scalarRowDual,
            C_NULL,
        )
        _check_ret(dest, ret)

        # COPT returns an error when COPT_GetPSDSolution() is called on a
        # non-SDP model.
        if nPSDColLen != 0 || nPSDRow != 0
            # Get PSD solution
            # Use undocumented COPT function
            # int COPT_GetPSDSolution(copt_prob* prob, double* psdColValue, double*
            #  psdRowSlack, double* psdRowDual, double* psdColDual)
            ret = ccall(
                (:COPT_GetPSDSolution, libcopt),
                Cint,
                (
                    Ptr{copt_prob},
                    Ptr{Cdouble},
                    Ptr{Cdouble},
                    Ptr{Cdouble},
                    Ptr{Cdouble},
                ),
                dest.prob,
                psdColVal,
                C_NULL,
                psdRowDual,
                C_NULL,
            )
            _check_ret(dest, ret)
        end

        # Recover primal and dual solution
        for i in 1:nScalarCol
            primalSol[i] = scalarColVal[i]
        end
        for i in 1:nPSDColLen
            primalSol[i+nScalarCol] = psdColVal[i]
        end
        for i in 1:nRow
            if outRowMap[i] < 0
                dualSol[i] = -psdRowDual[-outRowMap[i]]
            else
                dualSol[i] = -scalarRowDual[outRowMap[i]]
            end
        end

        objScale = ones(nCol)
        i = nFree + nPositive + nCone + nRotatedCone + 1
        for k = 1:nPSD
            for j = 1:psdDim[k]
                for l = i+1:i+psdDim[k]-j
                    objScale[l] = 2.0
                end
                i += psdDim[k] + 1 - j 
            end
        end

        objective_value =
            (max_sense ? 1 : -1) * LinearAlgebra.dot(b, dualSol) +
            objective_constant
        dual_objective_value =
            (max_sense ? 1 : -1) * LinearAlgebra.dot(c.*objScale, primalSol) +
            objective_constant
              
        dest.solution = ConeSolution(
            primalSol,
            dualSol,
            c - A * dualSol,
            objective_value,
            dual_objective_value,
            lpStatus,
        )

    end

    return index_map, false
end

function MOI.optimize!(
    dest::ConeOptimizer,
    src::MOI.Utilities.UniversalFallback{OptimizerCache},
)
    MOI.Utilities.throw_unsupported(src)
    return MOI.optimize!(dest, src.model)
end

function MOI.optimize!(dest::ConeOptimizer, src::MOI.ModelLike)
    cache = OptimizerCache()
    index_map = MOI.copy_to(cache, src)
    MOI.optimize!(dest, cache)
    return index_map, false
end

function MOI.get(model::ConeOptimizer, attr::MOI.RawStatusString)
    _throw_if_optimize_in_progress(model, attr)
    return _raw_lpstatus(model)[2]
end

function MOI.get(model::ConeOptimizer, attr::MOI.TerminationStatus)
    _throw_if_optimize_in_progress(model, attr)
    return _raw_lpstatus(model)[1]
end

MOI.get(model::ConeOptimizer, ::MOI.ResultCount) = 1

function MOI.get(
    model::ConeOptimizer,
    attr::Union{MOI.PrimalStatus,MOI.DualStatus},
)
    if attr.result_index > MOI.get(model, MOI.ResultCount()) ||
       model.solution isa Nothing
        return MOI.NO_SOLUTION
    end
    lpStatus = model.solution.lpStatus
    if lpStatus == COPT_LPSTATUS_OPTIMAL
        return MOI.FEASIBLE_POINT
    end
    if lpStatus == COPT_LPSTATUS_IMPRECISE
        return MOI.NEARLY_FEASIBLE_POINT
    end
    return MOI.NO_SOLUTION
end

function _check_solution_available(model::ConeOptimizer)
    if model.solution == nothing
        error("No solution available")
    end
end

function MOI.get(model::ConeOptimizer, attr::MOI.ObjectiveValue)
    MOI.check_result_index_bounds(model, attr)
    _check_solution_available(model)
    return model.solution.objective_value
end

function MOI.get(model::ConeOptimizer, attr::MOI.DualObjectiveValue)
    MOI.check_result_index_bounds(model, attr)
    _check_solution_available(model)
    return model.solution.dual_objective_value
end

function MOI.get(
    model::ConeOptimizer,
    attr::MOI.VariablePrimal,
    vi::MOI.VariableIndex,
)
    MOI.check_result_index_bounds(model, attr)
    _check_solution_available(model)
    return model.solution.y[vi.value]
end

function MOI.get(
    model::ConeOptimizer,
    attr::MOI.ConstraintPrimal,
    ci::MOI.ConstraintIndex,
)
    MOI.check_result_index_bounds(model, attr)
    _check_solution_available(model)
    return model.solution.slack[MOI.Utilities.rows(model.cones, ci)]
end

function MOI.get(
    model::ConeOptimizer,
    attr::MOI.ConstraintDual,
    ci::MOI.ConstraintIndex,
)
    MOI.check_result_index_bounds(model, attr)
    _check_solution_available(model)
    return model.solution.x[MOI.Utilities.rows(model.cones, ci)]
end
