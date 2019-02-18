Package["ComputationalOptics`"]


PackageImport["GeneralUtilities`"]


PackageScope["objectOptionValue"]
objectOptionValue[obj_,opt_]:=Lookup[Options@obj,opt]
