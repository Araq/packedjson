## Benchmark packedjson again stdlib's json:

when defined(useStdlib):
  import json
else:
  import ".." / packedjson
import strutils, times

proc main =
  let jobj = parseFile("1.json")

  let coordinates = jobj["coordinates"]
  let len = float(coordinates.len)
  doAssert coordinates.len == 1000000
  var x = 0.0
  var y = 0.0
  var z = 0.0

  for coord in coordinates:
    x += coord["x"].getFloat
    y += coord["y"].getFloat
    z += coord["z"].getFloat

  echo x / len
  echo y / len
  echo z / len

let start = cpuTime()
main()
echo "used Mem: ", formatSize getOccupiedMem(), " time: ", cpuTime() - start, "s"

#[
Results on my machine:

packed json:  used Mem: 94.06MiB time: 2.622s
old version:  used Mem: 140.063MiB time: 3.127s
stdlib json:  used Mem: 1.277GiB time: 3.759s

]#
