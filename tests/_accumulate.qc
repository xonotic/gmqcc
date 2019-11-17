void   (string str, ...)          print     = #1;
string (float val)                ftos      = #2;
entity ()                         spawn     = #3;
void   (entity ent)               kill      = #4;
string (vector vec)               vtos      = #5;
void   (string str)               error     = #6;
float  (vector vec)               vlen      = #7;
string (entity ent)               etos      = #8;
float  (string str)               stof      = #9;
string (...)                      strcat    = #10;
float  (string str1, string str2) strcmp    = #11;
vector (vector vec)               normalize = #12;
float  (float val)                sqrt      = #13;
float  (float val)                floor     = #14;
float  (float val1, float val2)   pow       = #15;
vector (string str)               stov      = #16;

[[accumulate]] float f() {
    print("a");
    return = 1;
}

[[accumulate]] float f() {
    print("b");
    return;
}

[[accumulate]] float f() {
    print("c");
    return 3;
}

[[accumulate]] float f() {
    print("d");
}

void main() {
    print(ftos(f()));
}
