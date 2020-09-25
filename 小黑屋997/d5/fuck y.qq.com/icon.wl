i = 0;
Export["Z:\\" <> ToString[i++] <> ".png", #]& /@ ImageCrop /@ Flatten[ImagePartition[..., Scaled[{1/9, 1/2}]]]
