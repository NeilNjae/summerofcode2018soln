#!/usr/bin/awk -f

BEGIN {
    x = 0
    y = 0
    d = 90
}

function forward (distance) {
    if (d == 0)   y += distance
    if (d == 90)  x += distance
    if (d == 180) y -= distance
    if (d == 270) x -= distance
}

function abs(v) {
    return v < 0 ? -v : v
    }

/^C/ {d = (d +  90) % 360}
/^A/ {d = (d + 270) % 360}
/^F[0-9]+/ {forward(substr($0, 2))}

END {
    print abs(x) + abs(y)
}
