Package["ComputationalOptics`"]

(*export*)
PackageScope["objectOptionValue"]


objectOptionValue[obj_,opt_]:=Lookup[Options@obj,opt]
