func f[x, y, z] {x x + x y z + y y + z z};
minimize[var f, 0.01, 1.0, 1.0, -1.0]