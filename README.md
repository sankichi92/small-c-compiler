# Small-C Compiler

Small-C Compiler converts [Small-C](https://en.wikipedia.org/wiki/Small-C) into [MIPS assembly language](https://en.wikipedia.org/wiki/MIPS_instruction_set).

## How to

Install [Racket](https://racket-lang.org/).

Then you can compile a Small-C file (`foo.sc`) into an assembly file (`foo.s`) by the following command.

```
$ ./scc foo.sc
```

(And you can execute MIPS assembly programs by [SPIM](http://spimsimulator.sourceforge.net/))

## Small-C

### Data Types

* `int`
* Arrays (one-dimentional)
* Pointers

### Control Flow

* `if/else`
* `while`
* `for`

### Operators

* arithmetic: `+`, `-`, `\*`, `/`
* assignment: `=`
* boolean logic: `&&`, `||`
* equality testing: `==`, `!=`
* order relations: `<`, `<=`, `>`, `>=`
* calling functions: `()`
* reference & dereference: `&`, `*`, `[]`
* sequencing: `,`
* subexpression grouping: `()`

### Sample <small>(./test/pgm/test.sc)</small>

```c
int f(int x) {
  while(x > 1) {
    x = x - 2;
  }
  return x;
}

void main() {
  int x;
  print(f(9));
}
```
