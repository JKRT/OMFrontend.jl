module ElementSource

import DAE
import ListUtil
using MetaModelica

"""
Constructs the element source.
TODO:
Currently a dummy impl
"""
function createElementSource(i)
  return DAE.emptyElementSource
end

"""
Merges two element sources into one.
Combines the connectEquationOptLst fields from both sources and keeps
all other fields from source1. This is sufficient for the connector
equation generation use cases in NFConnectEquations.
"""
function mergeSources(source1::DAE.ElementSource, source2::DAE.ElementSource)::DAE.ElementSource
  @match (source1, source2) begin
    (DAE.SOURCE(info = info,
                partOfLst = partOfLst,
                instance = instance,
                connectEquationOptLst = connects1,
                typeLst = typeLst,
                operations = operations,
                comment = comment),
     DAE.SOURCE(connectEquationOptLst = connects2)) => begin
      DAE.SOURCE(
        info,
        partOfLst,
        instance,
        listAppend(connects1, connects2),
        typeLst,
        operations,
        comment,
      )
    end
  end
end

end #= ElementSource =#
