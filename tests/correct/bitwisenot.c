// RUN: %tool "%s" > "%t"
// RUN: %diff %CORRECT "%t"

int foo()
{
    assert ~2 == -3;
    return 0;
}
