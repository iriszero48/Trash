import os
import vapoursynth as vs
import mvsfunc

core = vs.core

core.num_threads = 8
core.max_cache_size = 3000

vid_input = [

]

[os.remove(f'{x}.lwi') for x in vid_input if os.path.exists(f'{x}.lwi')]
[os.remove(f'{x}.ffindex') for x in vid_input if os.path.exists(f'{x}.ffindex')]

#vid = list(map(lambda x: core.lsmas.LWLibavSource(x, fpsnum=60, fpsden=1), vid_input))

vid = list(map(lambda x: core.ffms2.Source(source=x), vid_input))

size = min(map(len, vid))
vid = list(map(lambda x: x[:size], vid))

vid = [core.text.Text(vid[i], f"input{i}: {vid_input[i]}")
       for i in range(len(vid))]
       
vid = [mvsfunc.ToRGB(v, full=False, depth=8, matrix="bt709") for v in vid]

res = core.std.Interleave(vid)

res.set_output()
