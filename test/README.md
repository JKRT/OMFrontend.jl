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
