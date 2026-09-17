using Documenter, EDFPlus

DocMeta.setdocmeta!(EDFPlus, :DocTestSetup, :(using EDFPlus); recursive=true)

makedocs(;
    modules=[EDFPlus],
    authors="William Herrera and various patch submitters",
    sitename="EDFPlus.jl Documentation",
    repo=Documenter.Remotes.GitHub("wherrera10", "EDFPlus.jl"),
    format=Documenter.HTML(;
        canonical="https://wherrera10.github.io/EDFPlus.jl",
        edit_link="master",
        assets=String[],
    ),
    pages=[
        "Home" => "index.md",
        # Add future pages here, e.g., "API Reference" => "api.md"
    ],
)

deploydocs(;
    repo="github.com/wherrera10/EDFPlus.jl.git",
    devbranch="master",
)
