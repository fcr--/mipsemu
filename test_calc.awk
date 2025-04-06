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

BEGIN {
    add_test("0,0,add", "0")
    add_test("1,1,add", "2")
    add_test("1,-1,add", "0")
    add_test("-2,1,add", "-1")
    add_test("0x7fffffff,1,add", "-2147483648")
    add_test("-0x3fffffff,0x40000002,add", "3")
    run_tests()

    add_test("0,0,sub", "0")
    run_tests()
}
