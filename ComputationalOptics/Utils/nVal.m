Package["ComputationalOptics`"]

(*export*)
PackageScope["nVal"]


nVal[nums_?NumericQ]:=N[nums]
nVal[nums_?QuantityQ]:=N@QuantityMagnitude[nums]
nVal[nums:{(_?NumericQ)..}]:=N[nums]
nVal[nums_QuantityArray]:=N@QuantityMagnitude[nums]
nVal[nums_List/;CompatibleUnitQ[nums]]:=N@QuantityMagnitude@CommonUnits[nums]
nVal[nums_List/;!CompatibleUnitQ[nums]]:=ConstantArray[Indeterminate,Dimensions[nums]]
