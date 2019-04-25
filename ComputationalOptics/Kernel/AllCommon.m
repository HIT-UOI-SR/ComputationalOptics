Package["ComputationalOptics`"]


PackageImport["GeneralUtilities`"]


PackageScope["objectOptionValue"]
objectOptionValue[obj_,opt_]:=Lookup[Options@obj,opt]

PackageScopr["nVal"]
nVal[nums_]:=N@QuantityMagnitude@CommonUnits[nums]

PackageScope["realQ"]
realQ[expr_]:=NonNegative[expr]||NonPositive[expr]
