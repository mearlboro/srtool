// RUN: %tool "%s" > "%t"
// RUN: %diff %CORRECT "%t"



int foo()
{
    assert 1 > 3 < 4;
    return 0;
}
