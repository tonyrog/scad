// test children

module them(a)
{
    for (i = [0:$children-1]) {
	translate([10,10,10])
	    rotate([a*i,0,0])
	    children(i);
    }
}

them(45) {
    color("green") translate([20,0,0,]) cube(10);
    rotate([90,0,0]) translate([20,0,0]) color("blue") cube(20);
    color("red") scale([1,2,1]) cube(30);
}

