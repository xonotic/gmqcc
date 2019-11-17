void   (string str, ...)          print     = #1;
string (float val)                ftos      = #2;

float sum3(float a, float b, float c)
{
    return a + b + c;
}

typedef float(float, float, float) op3func_t;
var float(float a, float b, float c) f;
op3func_t g;

void main() {
    f = sum3;
    g = f;
    print(ftos(g(1, 2, 3)), "\n"); // prints 6
}
