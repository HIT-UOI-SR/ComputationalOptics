Package["ComputationalOptics`"]


PackageImport["GeneralUtilities`"]


PackageScope["objectOptionValue"]
objectOptionValue[obj_,opt_]:=Lookup[Options@obj,opt]

PackageScope["realNumberQ"]
realNumberQ[expr_]:=MatchQ[Quiet@N@expr,_Real]
