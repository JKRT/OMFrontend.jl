module Lapack

using MetaModelica
using ExportAll

#= /*
* This file is part of OpenModelica.
*
* Copyright (c) 1998-2014, Open Source Modelica Consortium (OSMC),
* c/o Linköpings universitet, Department of Computer and Information Science,
* SE-58183 Linköping, Sweden.
*
* All rights reserved.
*
* THIS PROGRAM IS PROVIDED UNDER THE TERMS OF GPL VERSION 3 LICENSE OR
* THIS OSMC PUBLIC LICENSE (OSMC-PL) VERSION 1.2.
* ANY USE, REPRODUCTION OR DISTRIBUTION OF THIS PROGRAM CONSTITUTES
* RECIPIENT'S ACCEPTANCE OF THE OSMC PUBLIC LICENSE OR THE GPL VERSION 3,
* ACCORDING TO RECIPIENTS CHOICE.
*
* The OpenModelica software and the Open Source Modelica
* Consortium (OSMC) Public License (OSMC-PL) are obtained
* from OSMC, either from the above address,
* from the URLs: http:www.ida.liu.se/projects/OpenModelica or
* http:www.openmodelica.org, and in the OpenModelica distribution.
* GNU version 3 is obtained from: http:www.gnu.org/copyleft/gpl.html.
*
* This program is distributed WITHOUT ANY WARRANTY; without
* even the implied warranty of  MERCHANTABILITY or FITNESS
* FOR A PARTICULAR PURPOSE, EXCEPT AS EXPRESSLY SET FORTH
* IN THE BY RECIPIENT SELECTED SUBSIDIARY LICENSE CONDITIONS OF OSMC-PL.
*
* See the full OSMC Public License conditions for more details.
*
*/ =#

function dgeev(
  inJOBVL::String,
  inJOBVR::String,
  inN::Integer,
  inA::List{<:List{<:AbstractFloat}},
  inLDA::Integer,
  inLDVL::Integer,
  inLDVR::Integer,
  inWORK::List{<:AbstractFloat},
  inLWORK::Integer,
)::Tuple{
  List{List{AbstractFloat}},
  List{AbstractFloat},
  List{AbstractFloat},
  List{List{AbstractFloat}},
  List{List{AbstractFloat}},
  List{AbstractFloat},
  Integer,
}
  local outINFO::Integer
  local outWORK::List{AbstractFloat}
  local outVR::List{List{AbstractFloat}}
  local outVL::List{List{AbstractFloat}}
  local outWI::List{AbstractFloat}
  local outWR::List{AbstractFloat}
  local outA::List{List{AbstractFloat}}

  @error "TODO: Defined in the runtime"
  return (outA, outWR, outWI, outVL, outVR, outWORK, outINFO)
end

function dgegv(
  inJOBVL::String,
  inJOBVR::String,
  inN::Integer,
  inA::List{<:List{<:AbstractFloat}},
  inLDA::Integer,
  inB::List{<:List{<:AbstractFloat}},
  inLDB::Integer,
  inLDVL::Integer,
  inLDVR::Integer,
  inWORK::List{<:AbstractFloat},
  inLWORK::Integer,
)::Tuple{
  List{AbstractFloat},
  List{AbstractFloat},
  List{AbstractFloat},
  List{List{AbstractFloat}},
  List{List{AbstractFloat}},
  List{AbstractFloat},
  Integer,
}
  local outINFO::Integer
  local outWORK::List{AbstractFloat}
  local outVR::List{List{AbstractFloat}}
  local outVL::List{List{AbstractFloat}}
  local outBETA::List{AbstractFloat}
  local outALPHAI::List{AbstractFloat}
  local outALPHAR::List{AbstractFloat}

  @error "TODO: Defined in the runtime"
  return (outALPHAR, outALPHAI, outBETA, outVL, outVR, outWORK, outINFO)
end

function dgels(
  inTRANS::String,
  inM::Integer,
  inN::Integer,
  inNRHS::Integer,
  inA::List{<:List{<:AbstractFloat}},
  inLDA::Integer,
  inB::List{<:List{<:AbstractFloat}},
  inLDB::Integer,
  inWORK::List{<:AbstractFloat},
  inLWORK::Integer,
)::Tuple{List{List{AbstractFloat}}, List{List{AbstractFloat}}, List{AbstractFloat}, Integer}
  local outINFO::Integer
  local outWORK::List{AbstractFloat}
  local outB::List{List{AbstractFloat}}
  local outA::List{List{AbstractFloat}}

  @error "TODO: Defined in the runtime"
  return (outA, outB, outWORK, outINFO)
end

function dgelsx(
  inM::Integer,
  inN::Integer,
  inNRHS::Integer,
  inA::List{<:List{<:AbstractFloat}},
  inLDA::Integer,
  inB::List{<:List{<:AbstractFloat}},
  inLDB::Integer,
  inJPVT::List{<:Integer},
  inRCOND::AbstractFloat,
  inWORK::List{<:AbstractFloat},
)::Tuple{
  List{List{AbstractFloat}},
  List{List{AbstractFloat}},
  List{Integer},
  Integer,
  Integer,
}
  local outINFO::Integer
  local outRANK::Integer
  local outJPVT::List{Integer}
  local outB::List{List{AbstractFloat}}
  local outA::List{List{AbstractFloat}}

  @error "TODO: Defined in the runtime"
  return (outA, outB, outJPVT, outRANK, outINFO)
end

function dgelsy(
  inM::Integer,
  inN::Integer,
  inNRHS::Integer,
  inA::List{<:List{<:AbstractFloat}},
  inLDA::Integer,
  inB::List{<:List{<:AbstractFloat}},
  inLDB::Integer,
  inJPVT::List{<:Integer},
  inRCOND::AbstractFloat,
  inWORK::List{<:AbstractFloat},
  inLWORK::Integer,
)::Tuple{
  List{List{AbstractFloat}},
  List{List{AbstractFloat}},
  List{Integer},
  Integer,
  List{AbstractFloat},
  Integer,
}
  local outINFO::Integer
  local outWORK::List{AbstractFloat}
  local outRANK::Integer
  local outJPVT::List{Integer}
  local outB::List{List{AbstractFloat}}
  local outA::List{List{AbstractFloat}}

  @error "TODO: Defined in the runtime"
  return (outA, outB, outJPVT, outRANK, outWORK, outINFO)
end

function dgesv(
  inN::Integer,
  inNRHS::Integer,
  inA::List{<:List{<:AbstractFloat}},
  inLDA::Integer,
  inB::List{<:List{<:AbstractFloat}},
  inLDB::Integer,
)::Tuple{List{List{AbstractFloat}}, List{Integer}, List{List{AbstractFloat}}, Integer}
  local outINFO::Integer
  local outB::List{List{AbstractFloat}}
  local outIPIV::List{Integer}
  local outA::List{List{AbstractFloat}}

  @error "TODO: Defined in the runtime"
  return (outA, outIPIV, outB, outINFO)
end

function dgglse(
  inM::Integer,
  inN::Integer,
  inP::Integer,
  inA::List{<:List{<:AbstractFloat}},
  inLDA::Integer,
  inB::List{<:List{<:AbstractFloat}},
  inLDB::Integer,
  inC::List{<:AbstractFloat},
  inD::List{<:AbstractFloat},
  inWORK::List{<:AbstractFloat},
  inLWORK::Integer,
)::Tuple{
  List{List{AbstractFloat}},
  List{List{AbstractFloat}},
  List{AbstractFloat},
  List{AbstractFloat},
  List{AbstractFloat},
  List{AbstractFloat},
  Integer,
}
  local outINFO::Integer
  local outWORK::List{AbstractFloat}
  local outX::List{AbstractFloat}
  local outD::List{AbstractFloat}
  local outC::List{AbstractFloat}
  local outB::List{List{AbstractFloat}}
  local outA::List{List{AbstractFloat}}

  @error "TODO: Defined in the runtime"
  return (outA, outB, outC, outD, outX, outWORK, outINFO)
end

function dgtsv(
  inN::Integer,
  inNRHS::Integer,
  inDL::List{<:AbstractFloat},
  inD::List{<:AbstractFloat},
  inDU::List{<:AbstractFloat},
  inB::List{<:List{<:AbstractFloat}},
  inLDB::Integer,
)::Tuple{
  List{AbstractFloat},
  List{AbstractFloat},
  List{AbstractFloat},
  List{List{AbstractFloat}},
  Integer,
}
  local outINFO::Integer
  local outB::List{List{AbstractFloat}}
  local outDU::List{AbstractFloat}
  local outD::List{AbstractFloat}
  local outDL::List{AbstractFloat}

  @error "TODO: Defined in the runtime"
  return (outDL, outD, outDU, outB, outINFO)
end

function dgbsv(
  inN::Integer,
  inKL::Integer,
  inKU::Integer,
  inNRHS::Integer,
  inAB::List{<:List{<:AbstractFloat}},
  inLDAB::Integer,
  inB::List{<:List{<:AbstractFloat}},
  inLDB::Integer,
)::Tuple{List{List{AbstractFloat}}, List{Integer}, List{List{AbstractFloat}}, Integer}
  local outINFO::Integer
  local outB::List{List{AbstractFloat}}
  local outIPIV::List{Integer}
  local outAB::List{List{AbstractFloat}}

  @error "TODO: Defined in the runtime"
  return (outAB, outIPIV, outB, outINFO)
end

function dgesvd(
  inJOBU::String,
  inJOBVT::String,
  inM::Integer,
  inN::Integer,
  inA::List{<:List{<:AbstractFloat}},
  inLDA::Integer,
  inLDU::Integer,
  inLDVT::Integer,
  inWORK::List{<:AbstractFloat},
  inLWORK::Integer,
)::Tuple{
  List{List{AbstractFloat}},
  List{AbstractFloat},
  List{List{AbstractFloat}},
  List{List{AbstractFloat}},
  List{AbstractFloat},
  Integer,
}
  local outINFO::Integer
  local outWORK::List{AbstractFloat}
  local outVT::List{List{AbstractFloat}}
  local outU::List{List{AbstractFloat}}
  local outS::List{AbstractFloat}
  local outA::List{List{AbstractFloat}}

  @error "TODO: Defined in the runtime"
  return (outA, outS, outU, outVT, outWORK, outINFO)
end

function dgetrf(
  inM::Integer,
  inN::Integer,
  inA::List{<:List{<:AbstractFloat}},
  inLDA::Integer,
)::Tuple{List{List{AbstractFloat}}, List{Integer}, Integer}
  local outINFO::Integer
  local outIPIV::List{Integer}
  local outA::List{List{AbstractFloat}}

  @error "TODO: Defined in the runtime"
  return (outA, outIPIV, outINFO)
end

function dgetrs(
  inTRANS::String,
  inN::Integer,
  inNRHS::Integer,
  inA::List{<:List{<:AbstractFloat}},
  inLDA::Integer,
  inIPIV::List{<:Integer},
  inB::List{<:List{<:AbstractFloat}},
  inLDB::Integer,
)::Tuple{List{List{AbstractFloat}}, Integer}
  local outINFO::Integer
  local outB::List{List{AbstractFloat}}

  @error "TODO: Defined in the runtime"
  return (outB, outINFO)
end

function dgetri(
  inN::Integer,
  inA::List{<:List{<:AbstractFloat}},
  inLDA::Integer,
  inIPIV::List{<:Integer},
  inWORK::List{<:AbstractFloat},
  inLWORK::Integer,
)::Tuple{List{List{AbstractFloat}}, List{AbstractFloat}, Integer}
  local outINFO::Integer
  local outWORK::List{AbstractFloat}
  local outA::List{List{AbstractFloat}}

  @error "TODO: Defined in the runtime"
  return (outA, outWORK, outINFO)
end

function dgeqpf(
  inM::Integer,
  inN::Integer,
  inA::List{<:List{<:AbstractFloat}},
  inLDA::Integer,
  inJPVT::List{<:Integer},
  inWORK::List{<:AbstractFloat},
)::Tuple{List{List{AbstractFloat}}, List{Integer}, List{AbstractFloat}, Integer}
  local outINFO::Integer
  local outTAU::List{AbstractFloat}
  local outJPVT::List{Integer}
  local outA::List{List{AbstractFloat}}

  @error "TODO: Defined in the runtime"
  return (outA, outJPVT, outTAU, outINFO)
end

function dorgqr(
  inM::Integer,
  inN::Integer,
  inK::Integer,
  inA::List{<:List{<:AbstractFloat}},
  inLDA::Integer,
  inTAU::List{<:AbstractFloat},
  inWORK::List{<:AbstractFloat},
  inLWORK::Integer,
)::Tuple{List{List{AbstractFloat}}, List{AbstractFloat}, Integer}
  local outINFO::Integer
  local outWORK::List{AbstractFloat}
  local outA::List{List{AbstractFloat}}

  @error "TODO: Defined in the runtime"
  return (outA, outWORK, outINFO)
end

@exportAll()
end
