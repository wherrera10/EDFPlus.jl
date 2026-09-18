# EDFPlus.jl

<img src="https://github.com/wherrera10/EDFPlus.jl/blob/master/docs/src/assets/eeg.png">

[![Build status](https://ci.appveyor.com/api/projects/status/cfw6pe03rfn9qsoo?svg=true)](https://ci.appveyor.com/project/wherrera10/edfplus.jl)
[![CI](https://github.com/wherrera10/EDFPlus.jl/actions/workflows/ci.yml/badge.svg)](https://github.com/wherrera10/EDFPlus.jl/actions/workflows/ci.yml)
[![Coverage Status](https://coveralls.io/repos/github/wherrera10/EDFPlus.jl/badge.svg)](https://coveralls.io/github/wherrera10/EDFPlus.jl)

Julia for handling BDF+ and EDF+ EEG and similar signal data files.

### Documentation

Here: https://wherrera10.github.io/EDFPlus.jl/dev/


### Installation:

To install from a Julia REPL command line session you may use ]add EDFPlus at the command line, or with Pkg:

    using Pkg
    Pkg.add("EDFPlus"))

Note that the test files that are downloaded by default include a 23 mb test file. Optional extra testing with Pkg.test includes data value testing that requires PyCall. If this is desired, see the test folder runtests.jl file for how to run cross-checking with pyedflib.

