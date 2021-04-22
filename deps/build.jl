@info "OMFrontend Starting build script"
import Pkg; Pkg.add("Pkg")
Pkg.add("OpenModelicaParser")
Pkg.build("OpenModelicaParser")
