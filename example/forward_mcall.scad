
foo(x=1);
z=1;

function f1(x) = x+1;

module foo(x) {
    bar(y=2);
    echo("cube", sz=sz, x=x);
    cube(sz);
}

module bar(y=2) {
    echo("sphere",y=y,z=f1(z));
    sphere(y);
}

baz();

module bar(y=3) {
    echo("sphere",y=y,z=z);
    sphere(y);
}

module baz() {
    echo("baz", x=f1(1));
    function f1(x) = x+2;

    function f1(x) = x+3;
}

z = 4;
sz = 2;
y=3;
