// RUN: %tool "%s" > "%t"
// RUN: %diff %CORRECT "%t"

int thing(int l) {
    assert 0 == 1 == 3 == 0;
    return 0;
}
