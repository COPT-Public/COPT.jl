#
# This bridge reorders DualExponential cones to conform with the MOI convention.
# It is based on ECOS.jl interface, with their copyright information copied 
# below. 
# 
# Copyright (c) 2014: Joao Felipe Santos, Iain Dunning, and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

struct ReorderedDualExponentialCone <: MOI.AbstractVectorSet end

Base.copy(set::ReorderedDualExponentialCone) = set
function MOI.Utilities.set_with_dimension(
    ::Type{ReorderedDualExponentialCone},
    dim,
)
    if dim != 3
        error(
            "Cannot create a `ReorderedDualExponentialCone` with dimension $dim.",
        )
    end
    return ReorderedDualExponentialCone()
end

struct ReorderedDualExponentialBridge{T,F} <:
       MOI.Bridges.Constraint.SetMapBridge{
    T,
    ReorderedDualExponentialCone,
    MOI.DualExponentialCone,
    F,
    F,
}
    constraint::MOI.ConstraintIndex{F,ReorderedDualExponentialCone}
end

function MOI.Bridges.Constraint.concrete_bridge_type(
    ::Type{<:ReorderedDualExponentialBridge{T}},
    F::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.DualExponentialCone},
) where {T}
    return ReorderedDualExponentialBridge{T,F}
end

function MOI.Bridges.map_set(
    ::Type{<:ReorderedDualExponentialBridge},
    ::MOI.DualExponentialCone,
)
    return ReorderedDualExponentialCone()
end

function MOI.Bridges.inverse_map_set(
    ::Type{<:ReorderedDualExponentialBridge},
    ::ReorderedDualExponentialCone,
)
    return MOI.DualExponentialCone()
end

function MOI.Bridges.map_function(
    ::Type{<:ReorderedDualExponentialBridge{T}},
    func,
) where {T}
    scalars = MOI.Utilities.eachscalar(func)
    return scalars[[3, 2, 1]]
end

# Swapping indices 1 <-> 3 is an involution (it is its own inverse)

function MOI.Bridges.inverse_map_function(
    BT::Type{<:ReorderedDualExponentialBridge},
    func,
)
    return MOI.Bridges.map_function(BT, func)
end

# It is also symmetric (it is its own transpose)

function MOI.Bridges.adjoint_map_function(
    BT::Type{<:ReorderedDualExponentialBridge},
    func,
)
    return MOI.Bridges.map_function(BT, func)
end

function MOI.Bridges.inverse_adjoint_map_function(
    BT::Type{<:ReorderedDualExponentialBridge},
    func,
)
    return MOI.Bridges.map_function(BT, func)
end
