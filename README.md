# COBOL OJT

## Build and Run

- First go to src folder in terminal 

```
    cd src
```

- Run the main program:
In Mac run this first 
```bash
mkdir -p bin && for f in *.cbl; do cobc -m "$f" -o "bin/$(basename "$f" .cbl).dylib"; done

```

In window run this frist 

```bash 
    if not exist bin mkdir bin && cobc -m *.cbl -o bin/
```
- And then run this 

```
    cobc -x user-main.cbl -o bin/user-main 
```

- For run the program run this in terminal 

```
   export COB_LIBRARY_PATH=bin
   ./bin/user-main
```


