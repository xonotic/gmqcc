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

//<graphitemaster> . always declares a field for all entities
//<graphitemaster> if you want to assign something to it, it's done on a per-entity basis
//<graphitemaster> .type fn_name(params) is an alias for .type(params) fn_name
//<graphitemaster> if you want a field that is a function returning a field you want something like `.(.type(...)(params) name`
//<graphitemaster> er, `.(.type(...))(params) name` I mean

// actually:
// `.type fn_name(params)` declares functions that return fields
// `.type(params) fn_name` declare fields that are functions that return `type`

float global_of_float;

.float field_of_float;

float(float f) fn_float_to_float_1;
float(float f) fn_float_to_float_1 { return f; }
float fn_float_to_float_2(float f);
float fn_float_to_float_2(float f) { return f; }

.float fn_float_to_field_of_float(float f);
.float fn_float_to_field_of_float(float f) { return field_of_float; }
//.float(float f) fn_float_to_field_of_float_bad { return field_of_float; }
.float(float f) field_of_fn_float_to_float;

.float(float f1) fn_float_to_field_of_fn_float_to_float(float f2);
.float(float f1) fn_float_to_field_of_fn_float_to_float(float f2) { return field_of_fn_float_to_float; }

var .float global_of_field_of_float;

var float(float f) global_of_fn_float_to_float_1;
var float global_of_fn_float_to_float_2(float f);

var .float global_of_fn_float_to_field_of_float(float f);

var .float(float f) todo1;

var .float(float f1) todo2(float f2);

void main() {

    entity e1 = spawn();

    // globals

    pn(fn_float_to_float_1(10));

    e1.(fn_float_to_field_of_float(5)) = 100;
    pn(e1.field_of_float);

    e1.field_of_fn_float_to_float = fn_float_to_float_1;
    pn(e1.field_of_fn_float_to_float(30));
    e1.field_of_fn_float_to_float = fn_float_to_float_2;
    pn(e1.field_of_fn_float_to_float(30));

    //pn(e1.fn_float_to_field_of_fn_float_to_float(6)(66));
    pn(e1.(fn_float_to_field_of_fn_float_to_float(6))(66));

    global_of_field_of_float = field_of_float;
    pn(e1.(global_of_field_of_float));

    global_of_fn_float_to_float_1 = fn_float_to_float_2;
    pn(global_of_fn_float_to_float_1(-10));

    global_of_fn_float_to_float_2 = fn_float_to_float_1;
    pn(global_of_fn_float_to_float_2(-10));

    global_of_fn_float_to_field_of_float = fn_float_to_field_of_float;
    pn(e1.(global_of_fn_float_to_field_of_float(-5)));

    p();

    // locals

    .float local_of_field_of_float = field_of_float;
    pn(e1.(local_of_field_of_float));

    float(float f) local_of_fn_float_to_float_1;
    local_of_fn_float_to_float_1 = fn_float_to_float_1;
    pn(local_of_fn_float_to_float_1(11));
    local_of_fn_float_to_float_1 = fn_float_to_float_2;
    pn(local_of_fn_float_to_float_1(11));

    float local_of_fn_float_to_float_2(float f);
    local_of_fn_float_to_float_2 = fn_float_to_float_1;
    pn(local_of_fn_float_to_float_2(12));
    local_of_fn_float_to_float_2 = local_of_fn_float_to_float_1;
    pn(local_of_fn_float_to_float_2(12));

    .float local_of_fn_float_to_field_of_float(float f);
    //local_of_fn_float_to_field_of_float = field_of_fn_float_to_float; // cannot assign .float(float) to .float(float)
    local_of_fn_float_to_field_of_float = fn_float_to_field_of_float;
    pn(e1.(local_of_fn_float_to_field_of_float(7)));

    .float(float f) local_of_field_of_fn_float_to_float;
    local_of_field_of_fn_float_to_float = field_of_fn_float_to_float;
    //local_of_field_of_fn_float_to_float = fn_float_to_field_of_float; // cannot assign .float(float) to .float(float)
    pn(e1.local_of_field_of_fn_float_to_float(13));

    .float(float f1) local_of_fn_float_to_field_of_fn_float_to_float(float f2);
    local_of_fn_float_to_field_of_fn_float_to_float = fn_float_to_field_of_fn_float_to_float;
    pn(e1.(local_of_fn_float_to_field_of_fn_float_to_float(8))(14));
}
