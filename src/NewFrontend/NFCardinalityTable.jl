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

module NFCardinalityTable

import ..System
import ..Util
import ..CONNECTOR
import ..toString
import ..INTEGER_EXPRESSION

using ExportAll
using MetaModelica

const Table = Dict{String, Int}

function emptyCardinalityTable()
  return Table()
end

function fromConnections(conns)
  local table::Table
  if System.getUsesCardinality()
    table = emptyCardinalityTable()
    for conn in conns.connections
      table = addConnector(conn.lhs, table)
      table = addConnector(conn.rhs, table)
    end
  else
    table = emptyCardinalityTable()
  end
  return table
end

function addConnector(conn::CONNECTOR, table::Table)
  local conn_str::String
  local count::Int
  conn_str = toString(conn)
    if (conn_str) in keys(table)
      count = table[conn_str]
      table[conn_str] = count + 1
    else
      table[conn_str] = 1
    end
  return table
end

function evaluateCardinality(@nospecialize(arg), table::Table)
  local res::Expression
  local count::Int
  local argStr = toString(arg)
  count = if argStr in keys(table)
    table[toString(arg)]
  else
    count = 0
  end
  return INTEGER_EXPRESSION(count)
end

"""
  Prints the table
"""
function print(table::Table)
  for p in table
    println(first(table) * ":"  * string(last(p)))
  end
end

@exportAll()
end
