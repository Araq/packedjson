# Package

version       = "0.2.2"
author        = "Araq"
description   = "packedjson is an alternative Nim implementation for JSON. The JSON is essentially kept as a single string in order to save memory over a more traditional tree representation."
license       = "MIT"

skipDirs = @["bench"]

# Dependencies

requires "nim >= 0.18.1"
