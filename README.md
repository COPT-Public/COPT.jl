# Using Julia interface for COPT (Cardinal Optimizer)

COPT (Cardinal Optimizer) is a mathematical optimization solver for large-scale optimization problems.
It includes high-performance solvers for LP, MIP, SOCP, convex QP/QCP and SDP.

## Prerequisite

COPT.jl requires Julia version 1.6 or above. Please apply for a free personal COPT license from [COPT application page](https://www.shanshu.ai/copt).

## Installation

Please install JuMP and COPT Julia interface from Julia with:

```julia
import Pkg
Pkg.add("JuMP")
Pkg.add("COPT")
```

When there is no COPT installed,
installing the COPT Julia interface will automatically download the necessary solver binaries.
Without a license, you can solve small models for non-commercial purpose.
We strongly recommend that you apply for a license by following the link above.

**Notes**<br>
**MacOS Apple M1/ARM:** on MacOS with Apple M1 chips, Intel based programs can run via *Rosetta*. When you installed the COPT binaries manually, then please make sure that the *COPT build* matches the *Julia build*. We recommend the Intel based COPT and Julia build, as the Apple M1/ARM build of Julia is experimental.

## Quick check

When the installation is done, you should be able to build and solve an LP problem with:

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

For solving SDP problems, you need to use `COPT.ConeOptimizer` in place of
`COPT.Optimizer`:

```julia
using JuMP
using COPT
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

Please refer to [Getting started with JuMP](https://jump.dev/JuMP.jl/stable/tutorials/getting_started/getting_started_with_JuMP/) 
for a quick introduction to writing and solving optimization models with JuMP.
The model above is adapted from the introduction page.
