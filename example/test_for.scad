$fn=75;

for (j = [0 : 5]) {
    translate([0,10,j])
      cylinder(100, r=0.5, center=true);
}

x = [for (j = [0 : 5]) cylinder(100, r=0.5, center=true)];

