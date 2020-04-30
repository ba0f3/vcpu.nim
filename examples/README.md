## Compilation
Runner will compile asm into bytecode on-the-fly and execute it

```shell
nim c -d:release runner
```

## Usage
runner requires cligen package

```shell
./runner ex01.asm
```

Output:
```
Hello world!
```

### example.asm
```shell
./runner --r0="pass pharse" example.asm
```

### strcmp.asm
```shell
./runner strcmp.asm --r0=str1 --r1=str2
```

### xor_encrypt.asm
```shell
./runner xor_encrypt.asm --r0=hello --r1=s3cret
```