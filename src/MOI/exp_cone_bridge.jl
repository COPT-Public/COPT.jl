#
# This bridge reorders exponentials cones to conform with the MOI convention.
# It is based on ECOS.jl interface, with their copyright information copied 
# below. 
# 
# Copyright (c) 2014: Joao Felipe Santos, Iain Dunning, and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# ECOS orders differently than MOI the second and third dimension of the
# exponential cone

struct ReorderedExponentialCone <: MOI.AbstractVectorSet end

Base.copy(set::ReorderedExponentialCone) = set
function MOI.Utilities.set_with_dimension(::Type{ReorderedExponentialCone}, dim)
    if dim != 3
        error("Cannot create a `ReorderedExponentialCone` with dimension $dim.")
    end
    return ReorderedExponentialCone()
end

struct ReorderedExponentialBridge{T,F} <: MOI.Bridges.Constraint.SetMapBridge{
    T,
    ReorderedExponentialCone,
    MOI.ExponentialCone,
    F,
    F,
}
    constraint::MOI.ConstraintIndex{F,ReorderedExponentialCone}
end

function MOI.Bridges.Constraint.concrete_bridge_type(
    ::Type{<:ReorderedExponentialBridge{T}},
    F::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.ExponentialCone},
) where {T}
    return ReorderedExponentialBridge{T,F}
end

function MOI.Bridges.map_set(
    ::Type{<:ReorderedExponentialBridge},
    ::MOI.ExponentialCone,
)
    return ReorderedExponentialCone()
end

function MOI.Bridges.inverse_map_set(
    ::Type{<:ReorderedExponentialBridge},
    ::ReorderedExponentialCone,
)
    return MOI.ExponentialCone()
end

function MOI.Bridges.map_function(
    ::Type{<:ReorderedExponentialBridge{T}},
    func,
) where {T}
    scalars = MOI.Utilities.eachscalar(func)
    return scalars[[3, 2, 1]]
end

# Swapping indices 1 <-> 3 is an involution (it is its own inverse)

function MOI.Bridges.inverse_map_function(
    BT::Type{<:ReorderedExponentialBridge},
    func,
)
    return MOI.Bridges.map_function(BT, func)
end

# It is also symmetric (it is its own transpose)

function MOI.Bridges.adjoint_map_function(
    BT::Type{<:ReorderedExponentialBridge},
    func,
)
    return MOI.Bridges.map_function(BT, func)
end

function MOI.Bridges.inverse_adjoint_map_function(
    BT::Type{<:ReorderedExponentialBridge},
    func,
)
    return MOI.Bridges.map_function(BT, func)
end
