module foo() {
    for (i=[0:$children-1]) {
	children(i);
    }
}

foo() {
    cube(1);
    echo("hello");
    cube(2);
}
