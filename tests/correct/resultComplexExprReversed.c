// RUN: %tool "%s" > "%t"
// RUN: %diff %CORRECT "%t"

int iffy(int i)
  requires i == 4,
  ensures 2 > \result
{
    return i > 3;
}
