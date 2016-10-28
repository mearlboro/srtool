// RUN: %tool "%s" > "%t"
// RUN: %diff %CORRECT "%t"

int x;
int y;

int foo(int i)
    requires x == 2,
    requires y == 3,
    ensures \old(y) > \old(x)
{
    y = 0;

    int old_x;
    old_x = 123;
    return 0;
}
