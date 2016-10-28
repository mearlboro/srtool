// RUN: %tool "%s" > "%t"
// RUN: %diff %CORRECT "%t"

//  using standart incrementing in SSA i -> i10, i1 -> i10
int iffy(int j)
{
    int i;
    i = j % 2;
    i = i >> 2;

    return 0;
}
