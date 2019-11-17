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

void p() {
    print("\n");
}

void pn(float n) {
    print(ftos(n), "\n");
}

// ======================================

.float x, y;

const .float mine, his = mine;

void pe(entity e) {
    print(ftos(e.x), ftos(e.y), ftos(e.mine), ftos(e.his), "\n");
}

void main() {
    entity e = spawn();

    pe(e);
    .float x_bak = x;
    e.x = 1;
    e.y = 2;
    e.mine = 3;
    pn(e.x_bak);
    pe(e);
    x = y;
    .float pf = mine;
    pn(e.pf);
    pe(e);
    e.his = 4;
    x = x_bak;
    pn(e.pf);
    pe(e);
}
