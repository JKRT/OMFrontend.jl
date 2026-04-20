#=
  MockAgentServer.jl
  ==================
  Minimal agent server for testing agentic_recompilation without a real LLM.

  Responds to /query with simple rule-based decisions for SimpleAgenticTest:
    - When x > 5.0 has fired, return y = -1.0 so the state starts shrinking.

  Start with:
    julia --project MockAgentServer.jl

  Requires HTTP and JSON3:
    julia -e 'import Pkg; Pkg.add(["HTTP","JSON3"])'
=#

using HTTP
using JSON3

const PORT = 8765

function handle_query(req::HTTP.Request)
  body   = JSON3.read(req.body)
  params = body["parameters"]
  state  = body["state"]
  t      = body["time"]

  @info "Agent queried at t=$t  state=$state  parameters=$params"

  # Rule: flip the rate to -1.0 so x starts shrinking after crossing 5.0
  new_values = Dict{String,Any}()
  for p in params
    new_values[string(p)] = -1.0
  end

  @info "Agent decision: $new_values"
  return HTTP.Response(200, ["Content-Type" => "application/json"],
                       JSON3.write(Dict("new_values" => new_values)))
end

function handle_register(req::HTTP.Request)
  body = JSON3.read(req.body)
  @info "Metamodel registered"
  return HTTP.Response(200, JSON3.write(Dict("status" => "ok")))
end

function handle_observe(req::HTTP.Request)
  return HTTP.Response(200, JSON3.write(Dict("status" => "ok")))
end

router = HTTP.Router()
HTTP.register!(router, "POST", "/query",    handle_query)
HTTP.register!(router, "POST", "/register", handle_register)
HTTP.register!(router, "POST", "/observe",  handle_observe)

@info "Mock agent server listening on port $PORT"
HTTP.serve(router, "0.0.0.0", PORT)
