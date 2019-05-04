Package["ComputationalOptics`"]


PackageImport["GeneralUtilities`"]


PackageScope["maybeLengthQ"]


PackageExport["LightField"]
SetUsage[LightField,
  "LightField[type$, data$] represents an object containing the light field information.",
  "LightField['Types'] gives a list of the vaild light field types.",
  "LightField[type$, 'Properties'] gives a list of the valid properties for the type$."
]

$types=<|
  "PlaneWave"->{"Data","Wavelength","PhysicalSize"}
|>
LightField["Types"]=Keys[$types]
LightField[type_String,"Properties"]:=With[
  {prop=$types[type]},
  If[MissingQ[prop],
    Missing["NotAvailable",type],
    prop
  ]
]

PackageExport["LightFieldQ"]
SetUsage[LightFieldQ,
  "LightFieldQ[expr$] yields True if expr$ is a valid LightField object."
]
HoldPattern@LightFieldQ[
  LightField["PlaneWave",
    KeyValuePattern[{
      "Data"->_?MatrixQ,
      "Wavelength"->_?maybeLengthQ,
      "PhysicalSize"->{_?maybeLengthQ,_?maybeLengthQ}
    }]
  ]
]=True
LightFieldQ[_]=False
