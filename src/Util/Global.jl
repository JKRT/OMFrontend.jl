module Global

global roots = ["",1,2]

const flagsIndex = 1

const recursionDepthLimit = 1024

function getGlobalRoot(index)
  @info "Getting global root"
  roots[index]
end

end
