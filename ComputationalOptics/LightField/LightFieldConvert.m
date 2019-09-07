Package["ComputationalOptics`"]

(*export*)
PackageExport["LightFieldConvert"]

(*import*)
PackageImport["GeneralUtilities`"]


SetAttributes[LightFieldConvert,ReadProtected]
GeneralUtilities`SetUsage[LightFieldConvert,
  "LightFieldConvert[field$, type$] convert light field$ to type$.",
  "LightFieldConvert[field$, type$, info$] convert light field$ to type$ with additional info$."
]

LightFieldConvert[obj_,_]:=obj(*empty implementation*)
LightFieldConvert[obj_,_,_]:=obj
