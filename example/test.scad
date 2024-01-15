$fn=75;

difference() {
    union() {
        translate([0,0,100])
            cylinder(30, r1=10.25, r2=5);
        difference() {
            cylinder(100, r=10.25);
            cylinder(100, r=8);
        }
        translate([0,0,-20])
            cylinder(20, r1=5, r2=10.25);
    }
   for (j = [0 : 5]) {
     translate([0, 0, 4+j*7])
     for (i = [0 : 360/20 : 360-360/20]) {
            rotate([90, 0, i])
            cylinder(100, r=0.5, center=true);
        }
    }
}

