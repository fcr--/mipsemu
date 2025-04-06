# external global:
#   name
# globals:
#   args
#   expected
#   next_section
#   stack_size
function run_tests( \
    cmd) {
    if (args == "") return
    printf "\033[1m%s:\033[0m %s -> %s: ", next_section, args, expected
    cmd = "LC_ALL=C awk -vhex=test_calc.hex -fmipsemu.awk -vargs=" args
    cmd | getline res
    if (res != ("stack: " expected)) {
        print "❌ error"
        printf "\033[41mreceived %s\033[0m\n", res
    } else {
        print "✅ ok"
    }
    close(cmd)
    args = ""
    expected = ""
}

function test_error(args, expected,
    code, cmd) {
    run_tests()
    printf "\033[1m%s (error expected):\033[0m %s -> %s: ", next_section, args, expected
    cmd = "LC_ALL=C awk -vhex=test_calc.hex -fmipsemu.awk -vargs=" args " 2>&1"
    code = (cmd | getline res)
    if (!code) {
        printf "❌ expected error\n\033[41mreceived %s\033[0m\n", res
    } else if (res != expected) {
        printf "❌ wrong error\n\033[41mreceived %s\033[0m\n", res
    } else {
        print "✅ ok"
    }
    close(cmd)
    args = ""
    expected = ""
}

function add_test(more_args, more_expected) {
    args = args "" (args != "" ? "," : "") more_args
    expected = expected "" (expected != "" ? "," : "") more_expected
}

function h2d(hex,
    dec, i, c) {
    hex = tolower(hex)
    for (i=1; i<=length(hex); i++) {
        c = index("0123456789abcdef", substr(hex, i, 1))
        if (!c) error("invalid hex number «" hex "»")
        dec = dec*16 + c-1
    }
    if (dec >= 2^31) dec -= 2^32  # make it signed
    return dec
}

function desc(test_name) {
    # filter which test to run with awk -ftest_calc.awk -vname=name1,name2,...
    # or: make tests name=name1,name2,...
    run_tests()
    next_section = test_name
    return name=="" || name ~ ("(^|,)" test_name "(,|$)")
}

function get_stack_size( \
    var) {
    while ((getline var<"test_calc.c"))
        if(sub(/^#define MAX_STACK_SIZE */, "", var) && match(var, /^[0-9]+/))
            stack_size = substr(var, 1, RLENGTH)
    if (!stack_size) {
        print "MAX_STACK_SIZE not found in test_calc.c" >"/dev/stderr"
        exit 1
    }
}

BEGIN {
    get_stack_size()

    if (desc("add")) {
        add_test("0,0,add", 0)
        add_test("1,1,add", 2)
        add_test("1,-1,add", 0)
        add_test("-2,1,add", -1)
        add_test("0x7fffffff,1,add", h2d("80000000"))
        add_test("-0x3fffffff,0x40000002,add", 3)
        # stack underflow test for pop() users is only going to be tested once here:
        test_error("1,add", "stack underflow")
    }

    if (desc("and")) {
        add_test("-1,-1,and", -1)
        add_test("-1,0,and", 0)
        add_test("0,-1,and", 0)
        add_test("0,0,and", 0)
        add_test("0x12345678,"h2d("87654321")",and", h2d("02244220"))
        add_test(h2d("ffffaa55")",0x55aaffff,and", h2d("55aaaa55"))
        add_test("0x12345678,-1,and", h2d("12345678"))
    }

    if (desc("bitshift")) {
        add_test("-1,0,bitshift", "-1")
        add_test("0x12345678,0,bitshift", h2d("12345678"))
        add_test("0,0,bitshift", 0)
        add_test("0x55aa55aa,1,bitshift", h2d("ab54ab54"))
        add_test("0x12345678,16,bitshift", h2d("56780000"))
        add_test("3,31,bitshift", h2d("80000000"))
        add_test("1,31,bitshift,1,bitshift", 0)
        add_test("0x12345678,-1,bitshift", h2d("91a2b3c"))
        add_test("-1,-1,bitshift", h2d("7fffffff"))
        add_test("-1,-16,bitshift", h2d("ffff"))
        add_test("0x12345678,-20,bitshift", h2d("123"))
        add_test("0xfff,22,bitshift,-21,bitshift", h2d("7fe"))
    }

    if (desc("copy")) {
        add_test("0,copy,1,2,2,copy", "1,2,1,2")
        add_test("4,5,0,copy", "4,5")
        test_error("1,-1,copy", "negative index")
        test_error("1,2,copy", "index underflow")
    }

    if (desc("idiv")) {
        add_test("-1,2,idiv", 0)
        add_test("-3,2,idiv", -1)
        add_test("42,-1,idiv", -42)
        add_test("0x12345678,0x10000,idiv", h2d("1234"))
        add_test("3,2,idiv", 1)
        test_error("3,0,idiv", "division by zero")
    }

    if (desc("eq")) {
        add_test("1,2,eq", 0)
        add_test("42,42,eq", -1)
        add_test("-7,-6,eq", 0)
        add_test("335,-335,eq", 0)
        add_test("0x55aa1234,dup,eq", -1)
        add_test("-1,-1,bitshift,dup,-1,eq,exch,1,31,bitshift,not,eq", "0,-1")
    }

    if (desc("or")) {
        add_test("3,6,or", 7)
        add_test("42,-1,or", -1)
        add_test("0x12345678,0,or", h2d("12345678"))
    }

    if (desc("sub")) {
        add_test("0,0,sub", 0)
        add_test("42,42,sub", 0)
        add_test("0x7fffffff,-1,sub", h2d("80000000"))
        add_test("-1,1,sub", -2)
    }

    if (desc("udiv")) {
        add_test("-1,2,udiv", h2d("7fffffff"))
        add_test("3,2,udiv", 1)
        add_test("42,-1,udiv", 0)
        test_error("3,0,udiv", "division by zero")
    }

    if (desc("zzz")) {
        add_test("-1,zzzext", h2d("ff"))
        add_test("0x12345678,zzzext", h2d("34"))
        add_test("-1,0,zzzins", h2d("ff00000f"))
        add_test("0,-1,zzzins", h2d("00fffff0"))
        add_test("0x12345678,0xabcdef,zzzins", h2d("12bcdef8"))
    }

    run_tests()
}
