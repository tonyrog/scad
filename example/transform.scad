
rotate([10,0,0]) cube(10);

rotate([20,0,0]) { cube(10); cube(20); }

rotate([30,0,0]) { color("red") cube(10); color("blue") cube(20); }

translate([-10,0,0]) union() {
    rotate([40,0,0]) color("black") cube(10);
    rotate([50,0,0]) color("green") cube(10);
};

