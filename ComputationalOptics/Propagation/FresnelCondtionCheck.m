Package["ComputationalOptics`"]

(*export*)
PackageScope["$FresnelConditionChecked"]
PackageScope["FresnelConditionCheck"]

(*import*)
PackageImport["GeneralUtilities`"]
PackageExport["Propagation"]


$FresnelConditionChecked=False
FresnelConditionCheck[lambda_,d_,w_,l_]/;$FresnelConditionChecked:=True
FresnelConditionCheck[lambda_,d_,w_,l_]:=Block[{},
  If[d^3<1/(16lambda)*(w^2+l^2)^2,
    Message[Propagation::frescond,d]
  ];
  True
]
