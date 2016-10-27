// RUN: %tool "%s" > "%t"
// RUN: %diff %CORRECT "%t"

int y;
int a;
int b;
int c;
int d;

int foo()
    requires b > 20
{
    assert a == 0;
    assert b >= 2;
    assert c == 0;
    assert d == 0;
    assert y == 0;
    return y - y;
}
