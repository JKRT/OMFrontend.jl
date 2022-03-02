# Tests for the frontend

Some useful commands:
```
for i in 1:18
	println("ctst$i = (ConnectTests.Connect$i), Connect$i, \"./Connectors/Connect$(i).mo\")")
end
```

## Help needed
In the Connectors folder there are several models testing various variants of Modelica connectors.
Not all of them are yet added to the testsuite, see the frontend validation tests.


## Development tips 
Sometimes, exception might lead to the test taking a long time to run.
To circumvent that, it is sometimes sufficient to take a single test case and run that directly outside of the declared tests.

For instance: 
```
@testset "Analog.Sources" begin
  prefix = "Modelica.Electrical.Analog.Sources"
  @test typeof(flattenModelInMSL("$(prefix).SignalVoltage")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL("$(prefix).ConstantVoltage")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL("$(prefix).SineVoltage")[1]) == OMFrontend.Main.FLAT_MODEL
  #Broken @test typeof(flattenModelInMSL("$(prefix).StepVoltage")[1]) == OMFrontend.Main.FLAT_MODEL
end
```

In the cose above say that the testsuite get's stuck on SineVoltage. 
To run this faster, just cancel the current run of the test and run SineVoltage outside (With the prefix defined):
```
  typeof(flattenModelInMSL("$(prefix).SineVoltage")[1]) == OMFrontend.Main.FLAT_MODEL
```
