// RUN: %tool "%s" > "%t"
// RUN: %diff %CORRECT "%t"

int iffy(int i)
  requires (i < 9999 && i > 0),
  ensures \result >= i
{
    int t;
    t = i;
    if(3 < 1 < 2) {
        t = i + 1;
    }
    return t;
}
