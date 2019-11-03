## 2x.vpy

import vapoursynth as vs
import mvsfunc as mvf
 
sourceFile = r"*.mp4"
core = vs.get_core(threads = 12)
core.max_cache_size = 8000
src = core.ffms2.Source(source = sourceFile)
src = mvf.Depth(src, depth = 32)
block_w = src.width / 10
block_h = src.height / 10
src = core.caffe.Waifu2x(
	clip = src,
	noise = 3,
	scale = 2,
	block_w = block_w,
	block_h = block_h,
	model = 6,
	cudnn = True,
	tta = False,
	batch = 1)
src = mvf.ToYUV(src, full = False, depth = 8, css = "420", matrix = 1)
src.set_output()
