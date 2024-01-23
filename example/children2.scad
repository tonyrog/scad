module move(x=0,y=0,z=0,rx=0,ry=0,rz=0)
{ translate([x,y,z])rotate([rx,ry,rz]) children(); }
   
move(10)           { cube(10,true); rotate([0,30,30]) color("red") cube(10,true); }
move(-10)          { color("gold") union() { cube(10,true); color("blue") sphere(d=11); } }
move(z=7.07, ry=45)cube(10,true);
move(z=-7.07,ry=45)cube(10,true);

