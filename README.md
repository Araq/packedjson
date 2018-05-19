# packedjson
packedjson is an alternative Nim implementation for JSON. The JSON is essentially kept as a single string in order to save memory over a more traditional tree representation.

# To compile the benchmark, run these commands:

```
nim c -r bench\generator

nim c -r -d:release bench\benchmark.nim
nim c -r -d:release -d:useStdlib benchmark.nim
```

On my machine, I got these results:

```
packed json:  used Mem: 94.06MiB time: 2.622s
stdlib json:  used Mem: 1.277GiB time: 3.759s
```

# Todo

- Make the API a complete drop-in replacement for ``json.nim``.
- Tests, tests, tests!
