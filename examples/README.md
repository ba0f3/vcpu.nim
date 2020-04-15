## Compilation
you will need NASM to compile
```shell
nasm ex01.asm
```

## Run
runner requires cligen package

```shell
nim c -d:release runner.nim
./runner ex01
```

Output:
```
Hello World!
```