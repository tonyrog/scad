// test of overriding symbols

foo();

module foo()
{
    x = 1;
    echo(x=x);
}

module foo()
{
    x = 2;
    echo(x=x);
}
