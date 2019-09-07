Package["ComputationalOptics`"]

(*export*)
PackageScope["realQ"]


realQ[expr_]:=NonNegative[expr]||NonPositive[expr]
