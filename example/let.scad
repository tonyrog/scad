echo(version=version());

// Simple example just outputting the function input parameters.
function f1(x, y) = echo("f1: ", x, y) 0.5 * x * x + 4 * y + 1;

echo(r1=f1(1, 2));

// To output the result, there are multiple possibilities, the
// easiest is to use let() to assign the result to a variable
// (y here) which is used for both echo() output and result.
function f2(x) = let(y = pow(x, 3)) echo("f2: ", y) y;

echo(r2=f2(4));

f3 = function (x) 10+x;

echo(r3=f3(4));

