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

float global_f;
.float field_f;

float global_af[5];
//float[5] global_af_2; NOPE

float(float) fn_f2f;
float fn_f2f_2(float);
float fn_f2f(float) { return 0; }
float fn_f2f_2(float a) { return a; }

.float() field_v2f_1;
.float(void) field_v2f_2;
.float(float) field_f2f;
.float fn_f2pf(float);
.float fn_f2pf(float) { return field_f; }

float fn_f_f2f_2_f(float a, float(float wat) f2f) {
    return f2f(a);
}

float fn_f_f2f_2_f_2(float a, float f2f(float wat)) {
    return f2f(a);
}

typedef .float(float) Fn_f2pf;
var Fn_f2pf var_fn_f2pf;
//var .float(float) var_fn_f2pf_2;
//.Fn_v2pf idk;
//..float() idk2;
//Fn_v2pf fn_v2pf() = { return field_f; }
//.Fn_v2pf field_v2pf;

typedef float(float) Fn_f2f;
typedef float ty_f;
typedef float ty_f5[5];

var float(float) var_fn_f2f;

float idk() { return 5; };
float idk2()() { return idk; }
float wtf(float, void) { return 6; }
float wtf2()(float, void) { return wtf; }

.float sanity;
//.float(float) madness { return sanity; }
void aaaaaa() {}
.float(.float(float a) a()()()) cthulhu(.float a)(float, void, ...)(.float a(float))()()()() { return aaaaaa; }

float unused;

void main() {
    //cthulhu(madness);

    unused = 1;

    global_f = 666;
    global_af[4] = 10;
    entity e = spawn();
    e.field_f = 667;
    //var_fn_f2pf_2 = fn_f2pf;
    //var_fn_f2pf = fn_f2pf;
    //e.field_v2pf = fn_v2pf;
    //e.idk();
    //idk = 6;
    //idk2 = 7;

    pn(global_f);
    pn(e.field_f);
    p();

    pn(fn_f2f(1));
    pn(fn_f2f_2(1));
    p();

    e.field_f2f = fn_f2f;
    pn(e.field_f2f(2));
    e.field_f2f = fn_f2f_2;
    pn(e.field_f2f(2));
    p();

    pn(e.(fn_f2pf(3)));
    p();

    .float loc_field_f = fn_f2pf(3);
    pn(e.loc_field_f);
    pn(e.(loc_field_f));
    p();

    var_fn_f2f = fn_f2f_2;
    pn(var_fn_f2f(4));
    p();

    pn(fn_f_f2f_2_f(5, fn_f2f_2));
    pn(fn_f_f2f_2_f_2(5, fn_f2f_2));
    p();

    float x, y = 1;
    //pn(x);
    pn(y);
}
