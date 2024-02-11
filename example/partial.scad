// partial evaluation

g = 1;

module part1()
{
    a = 1;
    b = 1+2;
    c = 3*(4+2);
}

//sin = function (x) 13;

module part2()
{
    a = PI;
    b = sin(90);
    c = sin(180) + sin(2*180);
    d = g + cos(180) + cos(360);
    e = tan(90);
    f = atan(tan(90));
    echo(a=a,b=b,g=g,sin=sin);
}

part2();
