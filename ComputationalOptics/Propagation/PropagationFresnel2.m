Package["ComputationalOptics`Propagation`"]

PackageExport["PropagationFresnel2"]
(*TODO: fix external*)

SetAttributes[PropagationFresnel2,ReadProtected]
GeneralUtilities`SetUsage[PropagationFresnel2,
  "PropagationFresnel2[input$, wavelength$, distance$, size$]\
  is a low-level method to calculate the propagation light field based on the Fresnel diffraction in the form of convolution."
]

tfFresnel2:=GeneralUtilities`Memoized@FunctionCompile@Function[{
  Typed[lambda,"Real64"],Typed[d,"Real64"],Typed[w,"Real64"],Typed[l,"Real64"],
  Typed[nw,"UnsignedMachineInteger"],Typed[nl,"UnsignedMachineInteger"]},
  Table[Exp[-Pi I lambda d (x^2+y^2)]*Exp[2Pi I d/lambda],
    {x,-nw/(2w),nw/(2w)-1.0/w,1.0/w},
    {y,-nl/(2l),nl/(2l)-1.0/l,1.0/l}
  ]
]
iPropagationFresnel2[input_,{lambda_,d_,w_,l_},{nw_,nl_}]:=
    opticalInverseFourier[opticalFourier[input]tfFresnel2[lambda,d,w,l,nw,nl]]
PropagationFresnel2[input_?MatrixQ,lambda_?Positive,d_?realQ]:=With[
  {sz=Dimensions[input]},
  {w=sz[[1]],l=sz[[-1]]},
  iPropagationFresnel2[input,nVal@{lambda,d,w,l},{w,l}]/;checkFresnelCond[lambda,d,w,l]
]
PropagationFresnel2[input_?MatrixQ,lambda_?Positive,d_?realQ,sz_?Positive]/;checkFresnelCond[lambda,d,sz,sz]:=
    iPropagationFresnel2[input,nVal@{lambda,d,sz,sz},Dimensions[input]]
PropagationFresnel2[input_?MatrixQ,lambda_?Positive,d_?realQ,{w_?Positive,l_?Positive}]/;checkFresnelCond[lambda,d,w,l]:=
    iPropagationFresnel2[input,nVal@{lambda,d,w,l},Dimensions[input]]
call_PropagationFresnel2:=(Message[Propagation::invarg,HoldForm@call];$Failed)

