Package["ComputationalOptics`"]


PackageImport["GeneralUtilities`"]


PackageScope["objectOptionValue"]
objectOptionValue[obj_,opt_]:=Lookup[Options@obj,opt]

PackageScope["nVal"]
nVal[nums_?NumericQ]:=N[nums]
nVal[nums_?QuantityQ]:=N@QuantityMagnitude[nums]
nVal[nums:{(_?NumericQ)..}]:=N[nums]
nVal[nums_QuantityArray]:=N@QuantityMagnitude[nums]
nVal[nums_List/;CompatibleUnitQ[nums]]:=N@QuantityMagnitude@CommonUnits[nums]
nVal[nums_List/;!CompatibleUnitQ[nums]]:=ConstantArray[Indeterminate,Dimensions[nums]]

PackageScope["realQ"]
realQ[expr_]:=NonNegative[expr]||NonPositive[expr]

PackageScope["lengthQuatityQ"]
lengthQuatityQ[expr_?QuantityQ]:=CompatibleUnitQ[expr,"Meters"]
lengthQuatityQ[_]=False
