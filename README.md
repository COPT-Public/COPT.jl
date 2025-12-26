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

## Options
All options are in line with user guide of version 7.1.1.
### Limits and tolerance
TimeLimit (seconds)
- Type: double
- Range: [0, 1e20]
- Default: 1e20

SolTimeLimit (seconds)
- Type: double
- Range: [0, 1e20]
- Default: 1e20

NodeLimit
- Type: integer
- Range: [-1, INT_MAX]
- Default: -1 (automatic)

BarIterLimit
- Type: integer
- Range: [0, INT_MAX]
- Default: 500

MatrixTol
- Type: double
- Range: [0, 1e-7]
- Default: 1e-10

FeasTol
- Type: double
- Range: [1e-9, 1e-4]
- Default: 1e-6

DualTol
- Type: double
- Range: [1e-9, 1e-4]
- Default: 1e-6

IntTol
- Type: double
- Range: [1e-9, 1e-1]
- Default: 1e-6

RelGap
- Type: double
- Range: [0, DBL_MAX]
- Default: 1e-4

AbsGap
- Type: double
- Range: [0, DBL_MAX]
- Default: 1e-6

### Presolve related
Presolve
- Type: integer
- Range:
  - -1: automatic
  - 0: off
  - 1: light and fast
  - 2: normal
  - 3: aggressive
  - 4: no limit utill no improvement
- Default: -1

Scaling
- Type: integer
- Range:
  - -1: automatic
  - 0: off
  - 1: on
- Default: -1

Dualize
- Type: integer
- Range:
  - -1: automatic
  - 0: off
  - 1: on
- Default: -1

### LP related
LpMethod
- Type: integer
- Range:
  - -1: automatic
  - 1: dual simplex
  - 2: interior
  - 3: crossover
  - 4: parallel (simplex & interior)
  - 5: simplex or interior
  - 6: PDLP
- Default: -1

DualPrice
- Type: integer
- Range:
  - -1: automatic
  - 0: Devex algorithm
  - 1: Steepest edge algorithm
- Default: -1

DualPerturb
- Type: integer
- Range:
  - -1: automatic
  - 0: no perturbation
  - 1: allow perturbation
- Default: -1

BarHomogeneous
- Type: integer
- Range:
  - -1: automatic
  - 0: off
  - 1: on
- Default: -1

BarOrder
- Type: integer
- Range:
  - -1: automatic
  - 0: Approximate Minimum Degree（AMD）algorithm
  - 1: Nested Dissection（ND）algorithm
- Default: -1

BarStart
- Type: integer
- Range:
  - -1: automatic
  - 0: Simple algorithm
  - 1: Mehrotra algorithm
  - 2: Modified Mehrotra algorithm
- Default: -1

Crossover
- Type: integer
- Range:
  - -1: automatic
  - 0: off
  - 1: on
- Default: -1

ReqFarkasRay
- Type: integer
- Range:
  - 0: off
  - 1: on
- Default: 0

### MIP related
CutLevel
- Type: integer
- Range:
  - -1: automatic
  - 0: off
  - 1: light and fast
  - 2: normal
  - 3: aggressive
- Default: -1

RootCutLevel
- Type: integer
- Range:
  - -1: automatic
  - 0: off
  - 1: light and fast
  - 2: normal
  - 3: aggressive
- Default: -1

TreeCutLevel
- Type: integer
- Range:
  - -1: automatic
  - 0: off
  - 1: light and fast
  - 2: normal
  - 3: aggressive
- Default: -1

RootCutRounds
- Type: integer
- Range: [-1, INT_MAX]
- Default: -1 (automatic)

NodeCutRounds
- Type: integer
- Range: [-1, INT_MAX]
- Default: -1 (automatic)

HeurLevel
- Type: integer
- Range:
  - -1: automatic
  - 0: off
  - 1: light and fast
  - 2: normal
  - 3: aggressive
- Default: -1

RoundingHeurLevel
- Type: integer
- Range:
  - -1: automatic
  - 0: off
  - 1: light and fast
  - 2: normal
  - 3: aggressive
- Default: -1

DivingHeurLevel
- Type: integer
- Range:
  - -1: automatic
  - 0: off
  - 1: light and fast
  - 2: normal
  - 3: aggressive
- Default: -1

SubMipHeurLevel
- Type: integer
- Range:
  - -1: automatic
  - 0: off
  - 1: light and fast
  - 2: normal
  - 3: aggressive
- Default: -1

FAPHeurLevel
- Type: integer
- Range:
  - -1: automatic
  - 0: off
  - 1: light and fast
  - 2: normal
  - 3: aggressive
- Default: -1

StrongBranching
- Type: integer
- Range:
  - -1: automatic
  - 0: off
  - 1: light and fast
  - 2: normal
  - 3: aggressive
- Default: -1

ConflictAnalysis
- Type: integer
- Range:
  - -1: automatic
  - 0: off
  - 1: on
- Default: -1

MipStartMode
- Type: integer
- Range:
  - -1: automatic
  - 0: off
  - 1: complete and feasible solution
  - 2: feasible solution
- Default: -1

MipStartNodeLimit
- Type: integer
- Range: [-1, INT_MAX]
- Default: -1 (automatic)

### SDP related
SDPMethod
- Type: integer
- Range:
  - -1: automatic
  - 0: primal-dual interior algorithm
  - 1: alternating direction method of multipliers algorithm
  - 2: dual interior algorithm
- Default: -1

### IIS related
IISMethod
- Type: integer
- Range:
  - -1: automatic
  - 0: quality first
  - 1: efficiency first
- Default: -1

### Feasibility relaxation related
FeasRelaxMode
- Type: integer
- Range:
  - 0: minimize weighted sum of conflicts
  - 1: optimal primal problem solution under minimized weighted sum of conflicts
  - 2: minimize sum of conflicts
  - 3: optimal primal problem solution under minimized sum of conflicts
  - 4: minimize sum of square of conflicts
  - 5: optimal primal problem solution under minimized sum of square of conflicts
- Default: 0

### parameters tuning related
TuneTimeLimit (seconds)
- Type: double
- Range: [0, 1e20]
- Default: 0 (automatic)

TuneTargetTime (seconds)
- Type: double
- Range: [0, DBL_MAX]
- Default: 1e-2

TuneTargetRelGap
- Type: double
- Range: [0, DBL_MAX]
- Default: 1e-4

TuneMethod
- Type: integer
- Range:
  - -1: automatic
  - 0: aggressive
  - 1: wider
- Default: -1

TuneMode
- Type: integer
- Range:
  - -1: automatic
  - 0: solving time
  - 1: optimal relative tolerance
  - 2: objective
  - 3: lower bound of objective
- Default: -1

TuneMeasure
- Type: integer
- Range:
  - -1: automatic
  - 0: average
  - 1: maximum
- Default: -1

TunePermutes
- Type: integer
- Range: [0, INT_MAX]
- Default: 0 (automatic)

TuneOutputLevel
- Type: integer
- Range:
  - 0: off
  - 1: summary of parameters
  - 2: summary of trails
  - 3: details of trails
- Default: 2

### Callbacks related
LazyConstraints (only active for MIPs)
- Type: integer
- Range:
  - -1: automatic
  - 0: off
  - 1: on
- Default: -1

### Parallel computation related
Threads
- Type: integer
- Range: [-1, 128]
- Default: -1 (automatic)

BarThreads
- Type: integer
- Range: [-1, 128]
- Default: -1 (automatic)

SimplexThreads
- Type: integer
- Range: [-1, 128]
- Default: -1 (automatic)

CrossoverThreads
- Type: integer
- Range: [-1, 128]
- Default: -1 (automatic)

MipTasks
- Type: integer
- Range: [-1, 256]
- Default: -1 (automatic)

### GPU computation related
GPUMode
- Type: integer
- Range:
  - -1: automatic
  - 0: CPU mode
  - 1: NVIDIA GPU mode
- Default: -1

GPUDevice
- Type: integer
- Range: [-1, INT_MAX]
- Default: -1 (automatic)

PDLPTol
- Type: double
- Range: [1e-12, 1e-4]
- Default: 1e-6

### Logging related
Logging
- Type: integer
- Range:
  - 0: off
  - 1: on
- Default: 1

LogToConsole
- Type: integer
- Range:
  - 0: off
  - 1: on
- Default: 1

LogFile
- Type: string
- Default:
