

## Setup

* download intellij community
* choose to install the scala plugin with it.

* ABOUT DOWNLOADNING THE SCALA SDK AND SHIZ

* import the project into intellij
* Check if the antlr dependency is properly added.  If it isnt, add it to the library (cmd+; -> libraries)


## Sam's bug list
* ternary exprs dont have semi colons

```
cat tests/correct/if.c | ./toSSA
int iffy(int i0)
ensures  (null>=i0) {

t0 = i0;
t1 =  (i0< ( (1<<24) ) )  ? t1 : t0
return t1;
}```
there is a null in the ensures(unknown var?)
