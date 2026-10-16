# EDFPlus.jl

<img src="https://github.com/wherrera10/EDFPlus.jl/blob/master/docs/src/assets/eeg.png">

[![Documentation Status](https://img.shields.io/badge/docs-latest-blue.svg)](https://wherrera10.github.io/EDFPlus.jl/)
[![CI](https://github.com/wherrera10/EDFPlus.jl/actions/workflows/ci.yml/badge.svg)](https://github.com/wherrera10/EDFPlus.jl/actions/workflows/ci.yml)

Julia for handling BDF+ and EDF+ EEG and similar signal data files. Documentation at https://wherrera10.github.io/EDFPlus.jl/


### Installation:

To install from a Julia REPL command line session you may use ]add EDFPlus at the command line, or with Pkg:

    using Pkg
    Pkg.add("EDFPlus"))

Note that the test files that are downloaded by default include a 23 mb test file. Optional extra testing with Pkg.test includes data value testing that requires PyCall. If this is desired, see the test folder runtests.jl file for how to run cross-checking on pyedflib.

