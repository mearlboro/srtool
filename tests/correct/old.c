// RUN: %tool "%s" > "%t"
// RUN: %diff %INCORRECT "%t"

int x;
int y;

int foo(int i)
    requires x == 2,
    requires y == 3,
    ensures \old(y) > \old(x)
{
    y = 0;
    return 0;
}
