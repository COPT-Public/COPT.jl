#
# Adapted from scaled_psd_cone_bridge.jl included in SCS.jl
#

struct ReorderedPSDCone <: MOI.AbstractVectorSet
    side_dimension::Int
end

function MOI.Utilities.set_with_dimension(::Type{ReorderedPSDCone}, dim)
    return ReorderedPSDCone(div(-1 + isqrt(1 + 8 * dim), 2))
end

Base.copy(x::ReorderedPSDCone) = ReorderedPSDCone(x.side_dimension)

MOI.side_dimension(x::ReorderedPSDCone) = x.side_dimension

function MOI.dimension(x::ReorderedPSDCone)
    return div(x.side_dimension * (x.side_dimension + 1), 2)
end

struct ReorderedPSDConeBridge{T,G} <: MOI.Bridges.Constraint.SetMapBridge{
    T,
    ReorderedPSDCone,
    MOI.PositiveSemidefiniteConeTriangle,
    MOI.VectorAffineFunction{T},
    G,
}
    constraint::MOI.ConstraintIndex{MOI.VectorAffineFunction{T},ReorderedPSDCone}
end

function MOI.Bridges.Constraint.concrete_bridge_type(
    ::Type{ReorderedPSDConeBridge{T}},
    ::Type{G},
    ::Type{MOI.PositiveSemidefiniteConeTriangle},
) where {T,G<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}}}
    return ReorderedPSDConeBridge{T,G}
end

function MOI.Bridges.map_set(
    ::Type{<:ReorderedPSDConeBridge},
    set::MOI.PositiveSemidefiniteConeTriangle,
)
    return ReorderedPSDCone(set.side_dimension)
end

function MOI.Bridges.inverse_map_set(
    ::Type{<:ReorderedPSDConeBridge},
    set::ReorderedPSDCone,
)
    return MOI.PositiveSemidefiniteConeTriangle(set.side_dimension)
end

function _upper_to_lower_triangular_permutation(dim::Int)
    side_dimension = MOI.Utilities.side_dimension_for_vectorized_dimension(dim)
    permutation = zeros(Int, dim)
    i = 0
    for row in 1:side_dimension
        start = div(row * (row + 1), 2)
        for col in row:side_dimension
            i += 1
            permutation[i] = start
            start += col
        end
    end
    return sortperm(permutation), permutation
end

function _transform_function(
    func::MOI.VectorAffineFunction{T},
    moi_to_copt::Bool,
) where {T}
    d = MOI.output_dimension(func)
    # upper_to_lower[i] maps the i'th element of the upper matrix to the linear
    #   index of the lower
    # lower_to_upper[i] maps the i'th element of the lower matrix to the linear
    #   index of the upper
    upper_to_lower, lower_to_upper = _upper_to_lower_triangular_permutation(d)
    reordered_constants = if moi_to_copt
        (func.constants)[lower_to_upper]
    else
        func.constants[upper_to_lower]
    end
    reordered_terms = MOI.VectorAffineTerm{T}[]
    for term in func.terms
        row = term.output_index
        i = moi_to_copt ? upper_to_lower[row] : lower_to_upper[row]
        push!(
            reordered_terms,
            MOI.VectorAffineTerm(
                i,
                MOI.ScalarAffineTerm(
                    term.scalar_term.coefficient,
                    term.scalar_term.variable,
                ),
            ),
        )
    end
    return MOI.VectorAffineFunction(reordered_terms, reordered_constants)
end

function _transform_function(func::MOI.VectorOfVariables, moi_to_copt)
    new_f = MOI.Utilities.operate(*, Float64, 1.0, func)
    return _transform_function(new_f, moi_to_copt)
end

function _transform_function(func::Vector{T},  moi_to_copt::Bool) where {T}
    d = length(func)
    upper_to_lower, lower_to_upper = _upper_to_lower_triangular_permutation(d)
    if moi_to_copt
        return func[lower_to_upper]
    else
        return func[upper_to_lower]
    end
end

# Map ConstraintFunction from MOI -> COPT
function MOI.Bridges.map_function(::Type{<:ReorderedPSDConeBridge}, f)
    return _transform_function(f, true)
end

# Used to map the ConstraintPrimal from COPT -> MOI
function MOI.Bridges.inverse_map_function(::Type{<:ReorderedPSDConeBridge}, f)
    return _transform_function(f, false)
end

# Used to map the ConstraintDual from COPT -> MOI
function MOI.Bridges.adjoint_map_function(::Type{<:ReorderedPSDConeBridge}, f)
    return _transform_function(f, false)
end

# Used to set ConstraintDualStart
function MOI.Bridges.inverse_adjoint_map_function(
    ::Type{<:ReorderedPSDConeBridge},
    f)
    return _transform_function(f, true)
end

