using Documenter
using OMFrontend

# Documenter pulls docstrings from loaded modules. Loading OMFrontend brings in
# its public API plus the Util/NewFrontend submodules that the API pages
# reference via @docs blocks.
DocMeta.setdocmeta!(OMFrontend, :DocTestSetup, :(using OMFrontend); recursive = true)

makedocs(
    sitename = "OMFrontend.jl",
    modules = [OMFrontend],
    authors = "John Tinnerholm <johti17@liu.se>, Martin Sjölund <martin.sjolund@liu.se>, Adrian Pop <adrian.pop@liu.se>",
    repo = Documenter.Remotes.GitHub("JKRT", "OMFrontend.jl"),
    format = Documenter.HTML(;
        prettyurls = get(ENV, "CI", "false") == "true",
        canonical = "https://JKRT.github.io/OMFrontend.jl",
        edit_link = "master",
        assets = String[],
    ),
    pages = [
        "Home" => "index.md",
        "Getting started" => "getting-started.md",
        "API reference" => "api.md",
    ],
    warnonly = [:missing_docs, :cross_references],
)

# Only deploy from CI; local `julia --project=docs docs/make.jl` just builds.
if get(ENV, "CI", "false") == "true"
    deploydocs(
        repo = "github.com/JKRT/OMFrontend.jl.git",
        devbranch = "master",
        push_preview = true,
    )
end
