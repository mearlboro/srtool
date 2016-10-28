// RUN: %tool "%s" > "%t"
// RUN: %diff %CORRECT "%t"

//  using standart incrementing in SSA i -> i10, i1 -> i10
int iffy()
{
    int i;
    i = 40;
    if(i < 60) {
        havoc i;
        assert i > 40 || i < 50;
        assume 1 != 1;
    }
    assert i == 40;

    return 0;
}
