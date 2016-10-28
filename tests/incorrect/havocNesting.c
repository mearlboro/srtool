// RUN: %tool "%s" > "%t"
// RUN: %diff %INCORRECT "%t"

//  using standart incrementing in SSA i -> i10, i1 -> i10
int iffy()
{
    int i;
    i = 40;
    if(i < 60) {
        havoc i;
        assert i > 40 || i < 50;
    }
    assert i == 40;

    return 0;
}
