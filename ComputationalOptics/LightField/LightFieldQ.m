Package["ComputationalOptics`"]

PackageExport["LightFieldQ"]
PackageScope["holdLightFieldQ"]

(*external*)
PackageImport["GeneralUtilities`"]
PackageExport["LightField"]
PackageScope["lengthQuatityQ"]


SetAttributes[LightFieldQ,ReadProtected]
GeneralUtilities`SetUsage[LightFieldQ,
  "LightFieldQ[expr$] yields True if expr$ is a valid LightField object.",
  "LightFieldQ[expr$, type$] yields True if expr$ is a valid LightField object of type$."
]

HoldPattern@LightFieldQ[
  LightField["MonochromaticPlaneComplex",
    KeyValuePattern[{
      "Data"->_?MatrixQ,
      "Wavelength"->_?lengthQuatityQ,
      "PhysicalSize"->{_?lengthQuatityQ,_?lengthQuatityQ}
    }]
  ]
]=True
LightFieldQ[_]=False
LightFieldQ[expr_,type_]:=LightFieldQ[expr] && expr["Type"]===type

SetAttributes[holdLightFieldQ,HoldAllComplete]
holdLightFieldQ[expr_]:=LightFieldQ[Unevaluated[expr]]
