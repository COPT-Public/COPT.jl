# Using Julia interface for COPT (Cardinal Optimizer)

COPT (Cardinal Optimizer) is a mathematical optimization solver for large-scale optimization problems.
It includes high-performance solvers for LP, MIP, SOCP, convex QP and convex QCP.

## Prerequisite

Please apply for free personal license from [COPT application page](https://www.shanshu.ai/copt).
Please make sure COPT is installed properly and you should be able to run examples in COPT package.


It requires COPT 4.0.5 or above. It works with Julia 1.6 or above.

## Installation

Please install JuMP and COPT Julia interface from Julia with:

```julia
import Pkg
Pkg.add("JuMP")
Pkg.add("COPT")
```

When there is no COPT installed,
installing COPT Julia interface will automatically download necessary solver binaries on Linux and MacOS.
Without a license, you can solve small models for non-commercial purpose.
We strongly recommend that you apply for a license by following the link above.

**Notes**<br>
**Windows:** we are experimenting automatic download for Windows too. It doesn’t quite work yet. So, you will need to install the COPT fully by following the user guide to be able to use the interface on Windows.<br>
**MacOS Apple M1/ARM:** on MacOS with Apple M1 chips, Intel based program can run via *Rosetta*. Please make sure that the *COPT build* matches the *Julia build*. We recommend the Intel based COPT and Julia build, as the Apple M1/ARM build of Julia is experimental.

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

Please refer to [Getting started with JuMP](https://jump.dev/JuMP.jl/stable/tutorials/getting_started/getting_started_with_JuMP/) 
for a quick introduction to writing and solving optimization models with JuMP.
The model above is adapted from the introduction page.
