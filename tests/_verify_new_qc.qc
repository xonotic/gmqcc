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

// note: the comments are from wiki, they're wrong
// https://gitlab.com/xonotic/xonotic/wikis/NewQC

float global; // global variable
//float .foo; // entity field

.float field; // fieldpointer
//.float .foo; // entity field of type fieldpointer

float function_void_to_float(void) { return 5; } // function
//float foo*(void); // function pointer

.float foo(void) { return field; } // function returning a fieldpointer .float
//.float foo*(void); // function pointer, returning a fieldpointer .float

//float .foo(void); // entity field of type function returning float
//.float .foo(void); // entity field of type function returning fieldpointer

void main() {
    foo();
}
