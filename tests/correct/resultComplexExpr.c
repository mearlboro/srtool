// RUN: %tool "%s" > "%t"
// RUN: %diff %CORRECT "%t"

int iffy(int i)
  requires i < 100,
  ensures \result > i
{
    return 1 + 2 - 2 + i + 1 - 1;
}
