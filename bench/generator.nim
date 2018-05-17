# Ported the script from Ruby to Nim.
# Original here: https://github.com/kostya/benchmarks/blob/master/json/generate_json.rb

import json, random, strutils, sequtils

var x = %[]

for _ in 0 ..< 1_000_000:
  var alpha = toSeq('a'..'z')
  shuffle(alpha)
  let h = %*{
    "x": rand(1.0),
    "y": rand(1.0),
    "z": rand(1.0),
    "name": alpha[0..4].join & ' ' & $rand(10000),
    "opts": {"1": [1, true]},
  }
  x.add h

writeFile("1.json", pretty(%*{"coordinates": x, "info": "some info"}))
