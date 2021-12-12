@info "OMFrontend Starting build script"
import Pkg; Pkg.add("Pkg")
#= We use master directly..=#
Pkg.add(url="https://github.com/OpenModelica/OMParser.jl.git")
Pkg.build("OMParser")
#= We use master directly..=#
Pkg.add(url="https://github.com/OpenModelica/ImmutableList.jl.git")
