# COPT.jl

[COPT.jl](https://github.com/COPT-Public/COPT.jl) is a wrapper for the
[COPT (Cardinal Optimizer)](https://www.shanshu.ai/copt), a mathematical
optimization solver for large-scale optimization problems.

COPT includes high-performance solvers for LP, MIP, SOCP, convex QP/QCP and SDP.

## License

COPT.jl is licensed under the [MIT license](https://github.com/COPT-Public/COPT.jl/blob/main/LICENSE).

The underlying solver is a closed-source commercial product for which you must
[obtain a license](https://www.shanshu.ai/copt).

### Note

When COPT is upgraded to a newer version, you may see an error message such as
`ERROR: COPT error 4: Unable to create COPT environment`, which indicates that
you will need to reapply and upgrade your COPT license files as well.

## Installation

Install COPT using the Julia package manager

```julia
import Pkg
Pkg.add("COPT")
```

When there is no local version of COPT installed, installing COPT.jl will
automatically download the necessary solver binaries.

Without a license, you can solve small models for non-commercial purpose. We
strongly recommend that you apply for a license by following the link above.


## Use with JuMP

To use COPT with JuMP, use `COPT.Optimizer`:

```julia
using JuMP
using COPT
model = Model(COPT.Optimizer)
@variable(model, x >= 0)
@variable(model, 0 <= y <= 3)
@objective(model, Min, 12x + 20y)
@constraint(model, c1, 6x + 8y >= 100)
@constraint(model, c2, 7x + 12y >= 120)
print(model)
optimize!(model)
@show termination_status(model)
@show primal_status(model)
@show dual_status(model)
@show objective_value(model)
@show value(x)
@show value(y)
@show shadow_price(c1)
@show shadow_price(c2)
```

To use the semidefinite programming solver in COPT with JuMP, use
`COPT.ConeOptimizer`:

```julia
using JuMP
using COPT
using LinearAlgebra
model = Model(COPT.ConeOptimizer)
C = [1.0 -1.0; -1.0 2.0]
@variable(model, X[1:2, 1:2], PSD)
@variable(model, z[1:2] >= 0)
@objective(model, Min, C ⋅ X)
@constraint(model, c1, X[1, 1] - z[1] == 1)
@constraint(model, c2, X[2, 2] - z[2] == 1)
optimize!(model)
@show termination_status(model)
@show primal_status(model)
@show dual_status(model)
@show objective_value(model)
@show value.(X)
@show value.(z)
@show shadow_price(c1)
@show shadow_price(c2)
```
