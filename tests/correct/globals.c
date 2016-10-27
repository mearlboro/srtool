// RUN: %tool "%s" > "%t"
// RUN: %diff %CORRECT "%t"

int y;
int a;
int b;
int c;
int d;

int foo() {
    d = 3;
    c = 4;

    d = c + d;
    c = d + c;

    assert c > d;
    return y - y;
}
