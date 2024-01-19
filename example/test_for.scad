$fn=75;

module m(i)
{
    for (j = [0 : 5]) cylinder(10+i, r=j+0.5, center=true);
}

for (j = [0 : 5]) {
    translate([j*10,10*j,j*10]) m(j);
}


