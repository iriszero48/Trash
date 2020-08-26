fi = 0;
Function[f,
  i = 1;
  path = "Z:\\test\\" <> ToString[fi];
  CreateDirectory[path];
  Function[x,
     Export[path  <> "\\" <> ToString[i] <> ".jpg", x];
    i++;
    ] /@ Flatten[ImagePartition[f, Scaled[1/4]]];
  fi++;
  ] /@ Import /@ FileNames["*", "Z:\\bh3"]
