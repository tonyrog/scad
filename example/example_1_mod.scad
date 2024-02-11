da = 5;
wa = 7;

module mycube(side=10, height=0, width=da+wa+1, depth=da+1) {
    h = height ? height : side;
    w = width ? width : side;
    d = depth ? depth : side;
    cube([h, w, d]);
}

// Create a cube with a height of 10
translate([-10,-10,-10]) mycube(height=20);
