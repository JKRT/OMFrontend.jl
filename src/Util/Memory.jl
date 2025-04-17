"""
Custom module to handle memory.
Similar to how the GC for OpenModelica starts with a specified size.
Here on the other hand we make use of the memory buffer, and we do it strictly for COMPONENT_NODE for experimental purposes.
"""
module MemoryUtil

using ManualMemory
using ManualMemory: MemoryBuffer, load, store!
import ..Frontend.Component
import ..Frontend.EMPTY_NODE
import ..Frontend.COMPONENT_NODE
import ..Frontend.EMPTY_COMPONENT
import ..Frontend.NORMAL_CLASS

#= Initial sketch.. =#
# global COMPONENT_NODE_BUFFER::MemoryBuffer
# global CUR_I::Int = 0
# global LAST_I::Int = 1000

# function initialize(N)
#   @info "Initializing the memory buffer..."
#   global CUR_I = 0
#   global LAST_I = N
#   global COMPONENT_NODE_BUFFER = MemoryBuffer{N, COMPONENT_NODE{String, Int}}(undef)
#   for i in 0:LAST_I-1
#     _setComponent!(COMPONENT_NODE_BUFFER, i,
#                    COMPONENT_NODE{String, Int}("dummy", 0, Ref{Component}(EMPTY_COMPONENT()), EMPTY_NODE(), NORMAL_CLASS()))
#   end
# end


# """
# If we have an allocated component available we update it and return it to the user.
# """
# function getComponent(buf, name, visibility, component, parent, nodeType)::COMPONENT_NODE
#   if CUR_I >= LAST_I
#     return COMPONENT_NODE{String, Int}(name, visibility, component, parent, nodeType)
#   end
#   local r = load(pointer(buf) + (i-1)*sizeof(COMPONENT_NODE))
#   r.name = name
#   r.visibility = visibility
#   r.component = component
#   r.parent = parent
#   r.nodeType = nodeType
#   CUR_I += 1
#   return r
# end

# """
# For internal use only. Sets component at position i.
# """
# function _setComponent!(buf, i, cn::COMPONENT_NODE)
#   store!(pointer(buf) + (i-1)*sizeof(COMPONENT_NODE{String, Int}), cn)
# end

#= Sketch2.. Bad but maybe gc does not steal my pointes this way TODO =#
#COMPONENT_NODE_BUFFER::Memory{COMPONENT_NODE{String, Int8}}= Memory{COMPONENT_NODE{String,Int}}(undef, 400)
# const CUR_POS::Ref{Int} = Ref{Int}(1)
# const LAST_POS::Ref{Int} = Ref{Int}(400)

# """
#   Initialize the component buffer with N possible components.
# """
# function initialize(N::Int = 400)::Nothing
#   #resize!([6, 5, 4, 3, 2, 1], COMPONENT_NODE_BUFFER)
#   #@info "Initializing the component cache..."
#   global CUR_POS.x = 1
#   global LAST_POS.x = N
#    for i in 1:N
#      COMPONENT_NODE_BUFFER[i] = COMPONENT_NODE{String, Int8}("d", 0, Ref{Component}(EMPTY_COMPONENT()), EMPTY_NODE(), NORMAL_CLASS())
#    end
#   nothing
# end

# """
#   Returns a unique component from the cache.
#   If there are no components available double the size of the cache and return the next.
# """
# function getNewComponent()::COMPONENT_NODE
#   if CUR_POS.x <= LAST_POS.x
#     comp = @inbounds COMPONENT_NODE_BUFFER[CUR_POS.x]
#     global CUR_POS.x += 1
#     return comp
#   else
#     @info "Resize cache..."
#     local newSize::Int = LAST_POS.x * 2
#     COMPONENT_NODE_BUFFER = Memory{COMPONENT_NODE{String,Int}}(undef, newSize)
#     global LAST_POS.x = newSize
#     for i in CUR_POS.x:LAST_POS.x
#       COMPONENT_NODE_BUFFER[i] = COMPONENT_NODE{String, Int8}(string("d", newSize),
#                                                               0,
#                                                               Ref{Component}(EMPTY_COMPONENT()),
#                                                               EMPTY_NODE(),
#                                                               NORMAL_CLASS())
#     end
#     comp = COMPONENT_NODE_BUFFER[CUR_POS.x]
#     global CUR_POS.x = CUR_POS.x + 1
#     return comp
#   end
# end

end
