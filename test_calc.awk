function run_tests( \
    cmd) {
    printf "%s -> %s: ", args, expected
    cmd = "LC_ALL=C awk -vhex=test_calc.hex -fmipsemu.awk -vargs=" args
    cmd | getline res
    if (res != ("stack: " expected)) {
        print "error"
        printf "received %s\n", res >"/dev/stderr"
        exit 1
    } else {
        print "ok"
        close(cmd)
    }
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

BEGIN {
    add_test("0,0,add", 0)
    add_test("1,1,add", 2)
    add_test("1,-1,add", 0)
    add_test("-2,1,add", -1)
    add_test("0x7fffffff,1,add", h2d("80000000"))
    add_test("-0x3fffffff,0x40000002,add", 3)
    run_tests()

    add_test("-1,-1,and", -1)
    add_test("-1,0,and", 0)
    add_test("0,-1,and", 0)
    add_test("0,0,and", 0)
    add_test("0x12345678,"h2d("87654321")",and", h2d("02244220"))
    add_test(h2d("ffffaa55")",0x55aaffff,and", h2d("55aaaa55"))
    add_test("0x12345678,-1,and", h2d("12345678"))
    run_tests()

    add_test("0,0,sub", 0)
    add_test("42,42,sub", 0)
    add_test("0x7fffffff,-1,sub", h2d("80000000"))
    add_test("-1,1,sub", -2)
    run_tests()

    add_test("-1,zzzext", h2d("ff"))
    add_test("0x12345678,zzzext", h2d("34"))
    add_test("-1,0,zzzins", h2d("ff00000f"))
    add_test("0,-1,zzzins", h2d("00fffff0"))
    add_test("0x12345678,0xabcdef,zzzins", h2d("12bcdef8"))
    run_tests()
}
