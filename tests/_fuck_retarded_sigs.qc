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

void pn(float n) = {
    print(ftos(n), "\n");
}

// ======================================

void v2v() { print(ftos(5)); }
float v2f() { return 6; }
void f2v(float x) { print(ftos(x)); }
float f2f(float x) { return x; }

void ret_v2v()() { return v2v; }
void ret_v2f()() { return v2f; }
void ret_f2v(float)() { return f2v; }
void ret_f2f(float)() { return f2f; }

float ret()() { return v2f; }
float f(float)();// { return f2f; }

//float()() xxx { return ret; }

void() ret_v2f_2() { return v2f; }

void main() {
    void() local_v2v_1 = v2v;
    void() local_v2v_2 = ret_v2v();

    float() local_v2f_1 = v2f;
    //float() local_v2f_2 = ret_v2f();

    void(float) local_f2v_1 = f2v;
    void(float) local_f2v_2 = ret_f2v();

    float(float) local_f2f_1 = f2f;
    //float(float) local_f2f_2 = ret_f2f();

    pn(ret()());
    pn(f()(666));
    pn(f()(666));

    ret_v2f_2()();
}
