Package["ComputationalOptics`"]

(*export*)
PackageScope["lengthQuatityQ"]

(*import*)
PackageScope["realQ"]


lengthQuatityQ[expr_?QuantityQ]:=CompatibleUnitQ[expr,"Meters"]&&realQ[expr]
lengthQuatityQ[_]=False
