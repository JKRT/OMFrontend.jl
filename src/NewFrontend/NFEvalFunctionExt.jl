module NFEvalFunctionExt

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
import ..P_NFExpression
P_Expression = P_NFExpression
Expression = P_NFExpression.NFExpression

import ..NFEvalFunction
EvalFunction = NFEvalFunction
import ..NFEvalFunction.assignVariable
import ..NFCeval
Ceval = NFCeval
import ..P_NFType
P_M_Type = P_NFType
M_Type = NFType
import ..Lapack

function Lapack_dgeev(args::List{<:Expression})
  local jobvl::Expression
  local jobvr::Expression
  local n::Expression
  local a::Expression
  local lda::Expression
  local ldvl::Expression
  local ldvr::Expression
  local work::Expression
  local lwork::Expression
  local wr::Expression
  local wi::Expression
  local vl::Expression
  local vr::Expression
  local info::Expression
  local INFO::Int
  local LDA::Int
  local LDVL::Int
  local LDVR::Int
  local LWORK::Int
  local N::Int
  local JOBVL::String
  local JOBVR::String
  local A::List{List{AbstractFloat}}
  local VL::List{List{AbstractFloat}}
  local VR::List{List{AbstractFloat}}
  local WORK::List{AbstractFloat}
  local WR::List{AbstractFloat}
  local WI::List{AbstractFloat}

  @match list(jobvl, jobvr, n, a, lda, wr, wi, vl, ldvl, vr, ldvr, work, lwork, info) = args
  JOBVL = evaluateExtStringArg(jobvl)
  JOBVR = evaluateExtStringArg(jobvr)
  N = evaluateExtIntArg(n)
  A = evaluateExtRealMatrixArg(a)
  LDA = evaluateExtIntArg(lda)
  LDVL = evaluateExtIntArg(ldvl)
  LDVR = evaluateExtIntArg(ldvr)
  WORK = evaluateExtRealArrayArg(work)
  LWORK = evaluateExtIntArg(lwork)
  (A, WR, WI, VL, VR, WORK, INFO) =
    Lapack.dgeev(JOBVL, JOBVR, N, A, LDA, LDVL, LDVR, WORK, LWORK)
  assignVariableExt(a, makeRealMatrix(A))
  assignVariable(wr, makeRealArray(WR))
  assignVariable(wi, makeRealArray(WI))
  assignVariableExt(vl, makeRealMatrix(VL))
  assignVariableExt(vr, makeRealMatrix(VR))
  assignVariable(work, makeRealArray(WORK))
  return assignVariable(info, makeInteger(INFO))
end

function Lapack_dgegv(args::List{<:Expression})
  local jobvl::Expression
  local jobvr::Expression
  local n::Expression
  local a::Expression
  local lda::Expression
  local b::Expression
  local ldb::Expression
  local alphar::Expression
  local alphai::Expression
  local beta::Expression
  local vl::Expression
  local ldvl::Expression
  local vr::Expression
  local ldvr::Expression
  local work::Expression
  local lwork::Expression
  local info::Expression
  local JOBVL::String
  local JOBVR::String
  local N::Int
  local LDA::Int
  local LDB::Int
  local LDVL::Int
  local LDVR::Int
  local LWORK::Int
  local INFO::Int
  local A::List{List{AbstractFloat}}
  local B::List{List{AbstractFloat}}
  local VL::List{List{AbstractFloat}}
  local VR::List{List{AbstractFloat}}
  local WORK::List{AbstractFloat}
  local ALPHAR::List{AbstractFloat}
  local ALPHAI::List{AbstractFloat}
  local BETA::List{AbstractFloat}

  @match list(
    jobvl,
    jobvr,
    n,
    a,
    lda,
    b,
    ldb,
    alphar,
    alphai,
    beta,
    vl,
    ldvl,
    vr,
    ldvr,
    work,
    lwork,
    info,
  ) = args
  JOBVL = evaluateExtStringArg(jobvl)
  JOBVR = evaluateExtStringArg(jobvr)
  N = evaluateExtIntArg(n)
  A = evaluateExtRealMatrixArg(a)
  LDA = evaluateExtIntArg(lda)
  B = evaluateExtRealMatrixArg(b)
  LDB = evaluateExtIntArg(ldb)
  LDVL = evaluateExtIntArg(ldvl)
  LDVR = evaluateExtIntArg(ldvr)
  WORK = evaluateExtRealArrayArg(work)
  LWORK = evaluateExtIntArg(lwork)
  (ALPHAR, ALPHAI, BETA, VL, VR, WORK, INFO) =
    Lapack.dgegv(JOBVL, JOBVR, N, A, LDA, B, LDB, LDVL, LDVR, WORK, LWORK)
  assignVariable(alphar, makeRealArray(ALPHAR))
  assignVariable(alphai, makeRealArray(ALPHAI))
  assignVariable(beta, makeRealArray(BETA))
  assignVariableExt(vl, makeRealMatrix(VL))
  assignVariableExt(vr, makeRealMatrix(VR))
  assignVariable(work, makeRealArray(WORK))
  return assignVariable(info, makeInteger(INFO))
end

function Lapack_dgels(args::List{<:Expression})
  local trans::Expression
  local m::Expression
  local n::Expression
  local nrhs::Expression
  local a::Expression
  local lda::Expression
  local b::Expression
  local ldb::Expression
  local work::Expression
  local lwork::Expression
  local info::Expression
  local TRANS::String
  local M::Int
  local N::Int
  local NRHS::Int
  local LDA::Int
  local LDB::Int
  local LWORK::Int
  local INFO::Int
  local A::List{List{AbstractFloat}}
  local B::List{List{AbstractFloat}}
  local WORK::List{AbstractFloat}

  @match list(trans, m, n, nrhs, a, lda, b, ldb, work, lwork, info) = args
  TRANS = evaluateExtStringArg(trans)
  M = evaluateExtIntArg(m)
  N = evaluateExtIntArg(n)
  NRHS = evaluateExtIntArg(nrhs)
  A = evaluateExtRealMatrixArg(a)
  LDA = evaluateExtIntArg(lda)
  B = evaluateExtRealMatrixArg(b)
  LDB = evaluateExtIntArg(ldb)
  WORK = evaluateExtRealArrayArg(work)
  LWORK = evaluateExtIntArg(lwork)
  (A, B, WORK, INFO) = Lapack.dgels(TRANS, M, N, NRHS, A, LDA, B, LDB, WORK, LWORK)
  assignVariableExt(a, makeRealMatrix(A))
  assignVariableExt(b, makeRealMatrix(B))
  assignVariable(work, makeRealArray(WORK))
  return assignVariable(info, makeInteger(INFO))
end

function Lapack_dgelsx(args::List{<:Expression})
  local m::Expression
  local n::Expression
  local nrhs::Expression
  local a::Expression
  local lda::Expression
  local b::Expression
  local ldb::Expression
  local jpvt::Expression
  local rcond::Expression
  local rank::Expression
  local work::Expression
  local info::Expression
  local M::Int
  local N::Int
  local NRHS::Int
  local LDA::Int
  local LDB::Int
  local RANK::Int
  local INFO::Int
  local A::List{List{AbstractFloat}}
  local B::List{List{AbstractFloat}}
  local JPVT::List{Int}
  local RCOND::AbstractFloat
  local WORK::List{AbstractFloat}

  if listLength(args) == 12
    @match list(m, n, nrhs, a, lda, b, ldb, jpvt, rcond, rank, work, info) = args
  else
    @match list(m, n, nrhs, a, lda, b, ldb, jpvt, rcond, rank, work, _, info) = args
  end
  #=  Some older versions of the MSL calls dgelsx with an extra lwork argument.
  =#
  M = evaluateExtIntArg(m)
  N = evaluateExtIntArg(n)
  NRHS = evaluateExtIntArg(nrhs)
  A = evaluateExtRealMatrixArg(a)
  LDA = evaluateExtIntArg(lda)
  B = evaluateExtRealMatrixArg(b)
  LDB = evaluateExtIntArg(ldb)
  JPVT = evaluateExtIntArrayArg(jpvt)
  RCOND = evaluateExtRealArg(rcond)
  WORK = evaluateExtRealArrayArg(work)
  (A, B, JPVT, RANK, INFO) =
    Lapack.dgelsx(M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND, WORK)
  assignVariableExt(a, makeRealMatrix(A))
  assignVariableExt(b, makeRealMatrix(B))
  assignVariable(jpvt, makeIntegerArray(JPVT))
  assignVariable(rank, makeInteger(RANK))
  return assignVariable(info, makeInteger(INFO))
end

function Lapack_dgelsy(args::List{<:Expression})
  local m::Expression
  local n::Expression
  local nrhs::Expression
  local a::Expression
  local lda::Expression
  local b::Expression
  local ldb::Expression
  local jpvt::Expression
  local rcond::Expression
  local rank::Expression
  local work::Expression
  local lwork::Expression
  local info::Expression
  local M::Int
  local N::Int
  local NRHS::Int
  local LDA::Int
  local LDB::Int
  local RANK::Int
  local LWORK::Int
  local INFO::Int
  local A::List{List{AbstractFloat}}
  local B::List{List{AbstractFloat}}
  local JPVT::List{Int}
  local RCOND::AbstractFloat
  local WORK::List{AbstractFloat}

  @match list(m, n, nrhs, a, lda, b, ldb, jpvt, rcond, rank, work, lwork, info) = args
  M = evaluateExtIntArg(m)
  N = evaluateExtIntArg(n)
  NRHS = evaluateExtIntArg(nrhs)
  A = evaluateExtRealMatrixArg(a)
  LDA = evaluateExtIntArg(lda)
  B = evaluateExtRealMatrixArg(b)
  LDB = evaluateExtIntArg(ldb)
  JPVT = evaluateExtIntArrayArg(jpvt)
  RCOND = evaluateExtRealArg(rcond)
  WORK = evaluateExtRealArrayArg(work)
  LWORK = evaluateExtIntArg(lwork)
  (A, B, JPVT, RANK, WORK, INFO) =
    Lapack.dgelsy(M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND, WORK, LWORK)
  assignVariableExt(a, makeRealMatrix(A))
  assignVariableExt(b, makeRealMatrix(B))
  assignVariable(jpvt, makeIntegerArray(JPVT))
  assignVariable(rank, makeInteger(RANK))
  assignVariable(work, makeRealArray(WORK))
  return assignVariable(info, makeInteger(INFO))
end

function Lapack_dgesv(args::List{<:Expression})
  local n::Expression
  local nrhs::Expression
  local a::Expression
  local lda::Expression
  local ipiv::Expression
  local b::Expression
  local ldb::Expression
  local info::Expression
  local N::Int
  local NRHS::Int
  local LDA::Int
  local LDB::Int
  local INFO::Int
  local A::List{List{AbstractFloat}}
  local B::List{List{AbstractFloat}}
  local IPIV::List{Int}

  @match list(n, nrhs, a, lda, ipiv, b, ldb, info) = args
  N = evaluateExtIntArg(n)
  NRHS = evaluateExtIntArg(nrhs)
  A = evaluateExtRealMatrixArg(a)
  LDA = evaluateExtIntArg(lda)
  B = evaluateExtRealMatrixArg(b)
  LDB = evaluateExtIntArg(ldb)
  (A, IPIV, B, INFO) = Lapack.dgesv(N, NRHS, A, LDA, B, LDB)
  assignVariableExt(a, makeRealMatrix(A))
  assignVariable(ipiv, makeIntegerArray(IPIV))
  assignVariableExt(b, makeRealMatrix(B))
  return assignVariable(info, makeInteger(INFO))
end

function Lapack_dgglse(args::List{<:Expression})
  local m::Expression
  local n::Expression
  local p::Expression
  local a::Expression
  local lda::Expression
  local b::Expression
  local ldb::Expression
  local c::Expression
  local d::Expression
  local x::Expression
  local work::Expression
  local lwork::Expression
  local info::Expression
  local M::Int
  local N::Int
  local P::Int
  local LDA::Int
  local LDB::Int
  local LWORK::Int
  local INFO::Int
  local A::List{List{AbstractFloat}}
  local B::List{List{AbstractFloat}}
  local C::List{AbstractFloat}
  local D::List{AbstractFloat}
  local WORK::List{AbstractFloat}
  local X::List{AbstractFloat}

  @match list(m, n, p, a, lda, b, ldb, c, d, x, work, lwork, info) = args
  M = evaluateExtIntArg(m)
  N = evaluateExtIntArg(n)
  P = evaluateExtIntArg(p)
  A = evaluateExtRealMatrixArg(a)
  LDA = evaluateExtIntArg(lda)
  B = evaluateExtRealMatrixArg(b)
  LDB = evaluateExtIntArg(ldb)
  C = evaluateExtRealArrayArg(c)
  D = evaluateExtRealArrayArg(d)
  WORK = evaluateExtRealArrayArg(work)
  LWORK = evaluateExtIntArg(lwork)
  (A, B, C, D, X, WORK, INFO) =
    Lapack.dgglse(M, N, P, A, LDA, B, LDB, C, D, WORK, LWORK)
  assignVariableExt(a, makeRealMatrix(A))
  assignVariableExt(b, makeRealMatrix(B))
  assignVariable(c, makeRealArray(C))
  assignVariable(d, makeRealArray(D))
  assignVariable(x, makeRealArray(X))
  assignVariable(work, makeRealArray(WORK))
  return assignVariable(info, makeInteger(INFO))
end

function Lapack_dgtsv(args::List{<:Expression})
  local n::Expression
  local nrhs::Expression
  local dl::Expression
  local d::Expression
  local du::Expression
  local b::Expression
  local ldb::Expression
  local info::Expression
  local N::Int
  local NRHS::Int
  local LDB::Int
  local INFO::Int
  local DL::List{AbstractFloat}
  local D::List{AbstractFloat}
  local DU::List{AbstractFloat}
  local B::List{List{AbstractFloat}}

  @match list(n, nrhs, dl, d, du, b, ldb, info) = args
  N = evaluateExtIntArg(n)
  NRHS = evaluateExtIntArg(nrhs)
  DL = evaluateExtRealArrayArg(dl)
  D = evaluateExtRealArrayArg(d)
  DU = evaluateExtRealArrayArg(du)
  B = evaluateExtRealMatrixArg(b)
  LDB = evaluateExtIntArg(ldb)
  (DL, D, DU, B, INFO) = Lapack.dgtsv(N, NRHS, DL, D, DU, B, LDB)
  assignVariable(dl, makeRealArray(DL))
  assignVariable(d, makeRealArray(D))
  assignVariable(du, makeRealArray(DU))
  assignVariableExt(b, makeRealMatrix(B))
  return assignVariable(info, makeInteger(INFO))
end

function Lapack_dgbsv(args::List{<:Expression})
  local n::Expression
  local kl::Expression
  local ku::Expression
  local nrhs::Expression
  local ab::Expression
  local ldab::Expression
  local ipiv::Expression
  local b::Expression
  local ldb::Expression
  local info::Expression
  local N::Int
  local KL::Int
  local KU::Int
  local NRHS::Int
  local LDAB::Int
  local LDB::Int
  local INFO::Int
  local AB::List{List{AbstractFloat}}
  local B::List{List{AbstractFloat}}
  local IPIV::List{Int}

  @match list(n, kl, ku, nrhs, ab, ldab, ipiv, b, ldb, info) = args
  N = evaluateExtIntArg(n)
  KL = evaluateExtIntArg(kl)
  KU = evaluateExtIntArg(ku)
  NRHS = evaluateExtIntArg(nrhs)
  AB = evaluateExtRealMatrixArg(ab)
  LDAB = evaluateExtIntArg(ldab)
  B = evaluateExtRealMatrixArg(b)
  LDB = evaluateExtIntArg(ldb)
  (AB, IPIV, B, INFO) = Lapack.dgbsv(N, KL, KU, NRHS, AB, LDAB, B, LDB)
  assignVariableExt(ab, makeRealMatrix(AB))
  assignVariable(ipiv, makeIntegerArray(IPIV))
  assignVariableExt(b, makeRealMatrix(B))
  return assignVariable(info, makeInteger(INFO))
end

function Lapack_dgesvd(args::List{<:Expression})
  local jobu::Expression
  local jobvt::Expression
  local m::Expression
  local n::Expression
  local a::Expression
  local lda::Expression
  local s::Expression
  local u::Expression
  local ldu::Expression
  local vt::Expression
  local ldvt::Expression
  local work::Expression
  local lwork::Expression
  local info::Expression
  local JOBU::String
  local JOBVT::String
  local M::Int
  local N::Int
  local LDA::Int
  local LDU::Int
  local LDVT::Int
  local LWORK::Int
  local INFO::Int
  local A::List{List{AbstractFloat}}
  local U::List{List{AbstractFloat}}
  local VT::List{List{AbstractFloat}}
  local S::List{AbstractFloat}
  local WORK::List{AbstractFloat}

  @match list(jobu, jobvt, m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork, info) = args
  JOBU = evaluateExtStringArg(jobu)
  JOBVT = evaluateExtStringArg(jobvt)
  M = evaluateExtIntArg(m)
  N = evaluateExtIntArg(n)
  A = evaluateExtRealMatrixArg(a)
  LDA = evaluateExtIntArg(lda)
  LDU = evaluateExtIntArg(ldu)
  LDVT = evaluateExtIntArg(ldvt)
  WORK = evaluateExtRealArrayArg(work)
  LWORK = evaluateExtIntArg(lwork)
  (A, S, U, VT, WORK, INFO) =
    Lapack.dgesvd(JOBU, JOBVT, M, N, A, LDA, LDU, LDVT, WORK, LWORK)
  assignVariableExt(a, makeRealMatrix(A))
  assignVariable(s, makeRealArray(S))
  assignVariableExt(u, makeRealMatrix(U))
  assignVariableExt(vt, makeRealMatrix(VT))
  assignVariable(work, makeRealArray(WORK))
  return assignVariable(info, makeInteger(INFO))
end

function Lapack_dgetrf(args::List{<:Expression})
  local m::Expression
  local n::Expression
  local a::Expression
  local lda::Expression
  local ipiv::Expression
  local info::Expression
  local M::Int
  local N::Int
  local LDA::Int
  local INFO::Int
  local A::List{List{AbstractFloat}}
  local IPIV::List{Int}

  @match list(m, n, a, lda, ipiv, info) = args
  M = evaluateExtIntArg(m)
  N = evaluateExtIntArg(n)
  A = evaluateExtRealMatrixArg(a)
  LDA = evaluateExtIntArg(lda)
  (A, IPIV, INFO) = Lapack.dgetrf(M, N, A, LDA)
  assignVariableExt(a, makeRealMatrix(A))
  assignVariable(ipiv, makeIntegerArray(IPIV))
  return assignVariable(info, makeInteger(INFO))
end

function Lapack_dgetrs(args::List{<:Expression})
  local trans::Expression
  local n::Expression
  local nrhs::Expression
  local a::Expression
  local lda::Expression
  local ipiv::Expression
  local b::Expression
  local ldb::Expression
  local info::Expression
  local TRANS::String
  local N::Int
  local NRHS::Int
  local LDA::Int
  local LDB::Int
  local INFO::Int
  local A::List{List{AbstractFloat}}
  local B::List{List{AbstractFloat}}
  local IPIV::List{Int}

  @match list(trans, n, nrhs, a, lda, ipiv, b, ldb, info) = args
  TRANS = evaluateExtStringArg(trans)
  N = evaluateExtIntArg(n)
  NRHS = evaluateExtIntArg(nrhs)
  A = evaluateExtRealMatrixArg(a)
  LDA = evaluateExtIntArg(lda)
  IPIV = evaluateExtIntArrayArg(ipiv)
  B = evaluateExtRealMatrixArg(b)
  LDB = evaluateExtIntArg(ldb)
  (B, INFO) = Lapack.dgetrs(TRANS, N, NRHS, A, LDA, IPIV, B, LDB)
  assignVariableExt(b, makeRealMatrix(B))
  return assignVariable(info, makeInteger(INFO))
end

function Lapack_dgetri(args::List{<:Expression})
  local n::Expression
  local a::Expression
  local lda::Expression
  local ipiv::Expression
  local work::Expression
  local lwork::Expression
  local info::Expression
  local N::Int
  local LDA::Int
  local LWORK::Int
  local INFO::Int
  local A::List{List{AbstractFloat}}
  local IPIV::List{Int}
  local WORK::List{AbstractFloat}

  @match list(n, a, lda, ipiv, work, lwork, info) = args
  N = evaluateExtIntArg(n)
  A = evaluateExtRealMatrixArg(a)
  LDA = evaluateExtIntArg(lda)
  IPIV = evaluateExtIntArrayArg(ipiv)
  WORK = evaluateExtRealArrayArg(work)
  LWORK = evaluateExtIntArg(lwork)
  (A, WORK, INFO) = Lapack.dgetri(N, A, LDA, IPIV, WORK, LWORK)
  assignVariableExt(a, makeRealMatrix(A))
  assignVariable(work, makeRealArray(WORK))
  return assignVariable(info, makeInteger(INFO))
end

function Lapack_dgeqpf(args::List{<:Expression})
  local m::Expression
  local n::Expression
  local a::Expression
  local lda::Expression
  local jpvt::Expression
  local tau::Expression
  local work::Expression
  local info::Expression
  local M::Int
  local N::Int
  local LDA::Int
  local INFO::Int
  local A::List{List{AbstractFloat}}
  local JPVT::List{Int}
  local WORK::List{AbstractFloat}
  local TAU::List{AbstractFloat}

  @match list(m, n, a, lda, jpvt, tau, work, info) = args
  M = evaluateExtIntArg(m)
  N = evaluateExtIntArg(n)
  A = evaluateExtRealMatrixArg(a)
  LDA = evaluateExtIntArg(lda)
  JPVT = evaluateExtIntArrayArg(jpvt)
  WORK = evaluateExtRealArrayArg(work)
  (A, JPVT, TAU, INFO) = Lapack.dgeqpf(M, N, A, LDA, JPVT, WORK)
  assignVariableExt(a, makeRealMatrix(A))
  assignVariable(jpvt, makeIntegerArray(JPVT))
  assignVariable(tau, makeRealArray(TAU))
  return assignVariable(info, makeInteger(INFO))
end

function Lapack_dorgqr(args::List{<:Expression})
  local m::Expression
  local n::Expression
  local k::Expression
  local a::Expression
  local lda::Expression
  local tau::Expression
  local work::Expression
  local lwork::Expression
  local info::Expression
  local M::Int
  local N::Int
  local K::Int
  local LDA::Int
  local LWORK::Int
  local INFO::Int
  local A::List{List{AbstractFloat}}
  local TAU::List{AbstractFloat}
  local WORK::List{AbstractFloat}

  @match list(m, n, k, a, lda, tau, work, lwork, info) = args
  M = evaluateExtIntArg(m)
  N = evaluateExtIntArg(n)
  K = evaluateExtIntArg(k)
  A = evaluateExtRealMatrixArg(a)
  LDA = evaluateExtIntArg(lda)
  TAU = evaluateExtRealArrayArg(tau)
  WORK = evaluateExtRealArrayArg(work)
  LWORK = evaluateExtIntArg(lwork)
  (A, WORK, INFO) = Lapack.dorgqr(M, N, K, A, LDA, TAU, WORK, LWORK)
  assignVariableExt(a, makeRealMatrix(A))
  assignVariable(work, makeRealArray(WORK))
  return assignVariable(info, makeInteger(INFO))
end

function evaluateExtIntArg(arg::Expression)::Int
  local value::Int = getExtIntValue(Ceval.evalExp(arg))
  return value
end

function getExtIntValue(exp::Expression)::Int
  local value::Int

  value = begin
    @match exp begin
      INTEGER_EXPRESSION(__) => begin
        exp.value
      end

      EMPTY(__) => begin
        0
      end
    end
  end
  return value
end

function evaluateExtRealArg(arg::Expression)::AbstractFloat
  local value::AbstractFloat = getExtRealValue(Ceval.evalExp(arg))
  return value
end

function getExtRealValue(exp::Expression)::AbstractFloat
  local value::AbstractFloat

  value = begin
    @match exp begin
      REAL_EXPRESSION(__) => begin
        exp.value
      end

      EMPTY(__) => begin
        0.0
      end
    end
  end
  return value
end

function evaluateExtStringArg(arg::Expression)::String
  local value::String = getExtStringValue(Ceval.evalExp(arg))
  return value
end

function getExtStringValue(exp::Expression)::String
  local value::String

  value = begin
    @match exp begin
      STRING_EXPRESSION(__) => begin
        exp.value
      end

      EMPTY(__) => begin
        ""
      end
    end
  end
  return value
end

function evaluateExtIntArrayArg(arg::Expression)::List{Int}
  local value::List{Int}

  local expl::List{Expression}

  expl = arrayElements(Ceval.evalExp(arg))
  value = list(getExtIntValue(e) for e in expl)
  return value
end

function evaluateExtRealArrayArg(arg::Expression)::List{AbstractFloat}
  local value::List{AbstractFloat}

  local expl::List{Expression}

  expl = arrayElements(Ceval.evalExp(arg))
  value = list(getExtRealValue(e) for e in expl)
  return value
end

function evaluateExtRealMatrixArg(arg::Expression)::List{List{AbstractFloat}}
  local value::List{List{AbstractFloat}}

  local expl::List{Expression}
  local ty::M_Type

  @match ARRAY_EXPRESSION(ty = ty, elements = expl) = Ceval.evalExp(arg)
  #=  Some external functions don't make a difference between vectors and
  =#
  #=  matrices, so if the argument is a vector we convert it into a matrix.
  =#
  value = begin
    @match Type.dimensionCount(ty) begin
      1 => begin
        list(list(getExtRealValue(e)) for e in expl)
      end

      2 => begin
        list(
          list(getExtRealValue(e) for e in arrayElements(row))
          for row in expl
        )
      end
    end
  end
  return value
end

""" #= Some external functions doesn't differentiate between vector and matrices, so
   we might get back a Nx1 matrix when expecting a vector. In that case it needs
   to be converted back into a vector before assigning the variable. Otherwise
   this function just calls assignVariable, so it's only needed for matrix
   arguments. =#"""
function assignVariableExt(variable::Expression, value::Expression)
  local exp::Expression

  exp = begin
    @match (typeOf(variable), value) begin
      (
        TYPE_ARRAY(dimensions = _ <| nil()),
        ARRAY_EXPRESSION(ty = TYPE_ARRAY(dimensions = _ <| _ <| nil())),
      ) => begin
        makeArray(
          Type.unliftArray(value.ty),
          list(arrayScalarElement(e) for e in value.elements),
          literal = true,
        )
      end

      _ => begin
        value
      end
    end
  end
  #=  Vector variable, matrix value => convert value to vector.
  =#
  return assignVariable(variable, exp)
end

@exportAll()
end
