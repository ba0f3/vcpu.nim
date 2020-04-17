## Compilation
you will need NASM to compile
```shell
nasm ex01.asm
```

## Usage
runner requires cligen package

```shell
nim c -d:release runner.nim
./runner ex01
```

Output:
```
Hello World!
```

### example.asm
```shell
nasm example.asm
./runner --r0="pass pharse" example
```

### strcmp.asm
```shell
nasm strcmp.asm
./runner --r0=str1 --r1=str2 strcmp
```

### xor_encrypt.asm
```shell
nasm xor_encrypt.asm
./runner --r0=hello --r1=s3cret xor_encrypt
```