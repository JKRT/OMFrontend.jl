module NFCardinalityTable

using MetaModelica
using ExportAll
#= Forward declarations for uniontypes until Julia adds support for mutual recursion =#

FuncHash = Function
FuncEq = Function
FuncKeyStr = Function
FuncValueStr = Function

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
import ..BaseHashTable
import ..System
import ..Util
using MetaModelica

Key = String
Value = Int
Table = Tuple

function emptyCardinalityTable(size::Int)::Table
  local table::Table
  table = BaseHashTable.emptyHashTableWork(
    size,
    (stringHashDjb2Mod, stringEq, Util.id, intString),
  )
  return table
end

function fromConnections(conns)::Table
  local table::Table
  if System.getUsesCardinality()
    table =
      emptyCardinalityTable(max(1, Util.nextPrime(listLength(conns.connections))))
    for conn in conns.connections
      table = addConnector(conn.lhs, table)
      table = addConnector(conn.rhs, table)
    end
  else
    table = emptyCardinalityTable(1)
  end
  return table
end

function addConnector(conn, table::Table)::Table
  local conn_str::String
  local count::Int
  conn_str = Main.Connector.toString(conn)
  try
    count = BaseHashTable.get(conn_str, table)
    BaseHashTable.update((conn_str, count + 1), table)
  catch
    table = BaseHashTable.add((conn_str, 1), table)
  end
  return table
end

function evaluateCardinality(arg, table::Table)
  local res::Expression
  local count::Int
  try
    count = BaseHashTable.get(toString(arg), table)
  catch
    count = 0
  end
  res = INTEGER_EXPRESSION(count)
  return res
end

function print(table::Table)
  return for e in BaseHashTable.hashTableList(table)
    print(Util.tuple21(e) + ": " + String(Util.tuple22(e)) + "\\n")
  end
end

@exportAll()
end
