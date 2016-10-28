// RUN: %tool "%s" > "%t"
// RUN: %diff %CORRECT "%t"

//  using standart incrementing in SSA i -> i10, i1 -> i10
int iffy()
{
    int i;
    if(i < 4) {
        assume 1 != 1;
        assert 1 != 1;
    } else {
        if(i > 4) {
            assume i == 3;
            assert i == 5;
        } else {
            havoc i;
            assume i == 4000;
            assert i == 4000;

        }
    }

    return 0;
}
