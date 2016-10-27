// RUN: %tool "%s" > "%t"
// RUN: %diff %CORRECT "%t"

int foo() {

    assert 4  / 2 /2 == 1;
    return 0;

}
