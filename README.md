# Using Julia interface for COPT (Cardinal Optimizer)

## Prerequisite

First of all, please apply for free personal license from [COPT application page](https://www.shanshu.ai/copt).

Please make sure COPT is installed properly and you should be able to run examples in COPT package.

Please note that the COPT Julia interface can only work with COPT 4.0.6 or above.

## Installation

Please install JuMP and COPT Julia interface from Julia with:

```
import Pkg
Pkg.add("JuMP")
Pkg.add(url="https://github.com/COPT-Public/COPT.jl/")
```

## Quick check

When the installation is done, you should be able to build and solve an LP problem with:

```
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
The model above is adapt from the introduction page.
