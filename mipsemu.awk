BEGIN {
    INITIAL_SP =    0x300000  # @ 3MiB (a stack of 3MiB should be enough for everyone)
    # usual .org:   0x400000  # @ 4MiB
    VIDEO_RAM =   0x10000000  # @ 256MiB (768B for palette + 64000B for frame-buffer)
    GENERAL_RAM = 0x10010000  # @ 256MiB + 64kiB (grows upwards)
    REG_V0 = 2
    REG_V1 = 3
    REG_A0 = 4
    REG_A1 = 5
    REG_A2 = 6
    REG_S2 = 18
    REG_T9 = 25
    REG_GP = 28
    REG_SP = 29
    REG_RA = 31
}

# CPU:
#   0..31: (u32) General Purpose Registers, CPU[0]==0
#   "HI", "LO": (u32) registers used for the multiplication and division unit
#   "PC": (u32) instruction pointer
#   "NEXTPC": (u32|nil) value for the instruction pointer that should be used
#       after running next instruction
#   "TLS_TP": thread pointer, only one thread is supported.
#   "CYCLES": how many instructions were executed in total

# DEBUG: can be set by using -vdebug=[item1,item2,...]
#   "instr": list each instruction executed
#   "syscall": list syscalls executed with params
#   "writev": show the base and len of each iov in the writev syscall

function error(text) {
    print "\033[41mERROR:\033[0m " text >"/dev/stderr"
    exit 1
}

function band(x, y,
    res, i) {
    # bitwise add, always returns unsigned integer
    i = 1
    while (x && y) {
        if ((x % 2) && (y % 2)) res += i
        x = int(x/2)
        y = int(y/2)
        i *= 2
    }
    return res
}

function bor(x, y) {
    # dumb bitwise or, always returns unsigned integer
    return x - band(x, y) + y
}

function blshift(num, times) {
    return to_u32(num * 2^times)
}

function brshift(num, times) {
    # does not work properly for negative numbers
    return int(num / 2^times)
}

function barshift(num, times) {
    # does not work properly for negative numbers
    if (num < 0x80000000) return int(num / 2^times)
    return 0x100000000 - 2^times + int(num / 2^times)
}

function load_elf_file(filename, ELF,
    H2D, i, b, nline) {
    # reads the hex file at filename, loading the byte array at ELF, parsing
    # the header into the following ELF keys:
    # - entry: memory address where the program starts (.org)
    # - phoff: byte-offset into ELF for the program header start
    # - shoff: byte-offset into ELF for the section header start
    # - phnum: number of program header entries
    # - shnum: number of section header entries
    # - shstrndx: index of the section header entry containing section names

    # Dumb hex to decimal:
    for (i = 0; i <= 0xff; i++)
        H2D[sprintf("%02x", i)] = i

    # Load bytes:
    ELF["n"] = 0
    while ((getline <filename) > 0) {
        for (i = 1; i <= NF; i++) {
            b = H2D[tolower($i)]
            if (b == "") error("invalid line " $0 " at " $NR)
            ELF[ELF["n"]++] = b
        }
    }
    if (!ELF["n"]) error("make sure «"filename"» exists and is a valid ELF")

    # Parse the ELF header:
    # Assert known header "\177ELF" at 0:
    for (i = 0; i < 4; i++) {
        b = substr("\177ELF", i+1, 1)
        if (sprintf("%c", ELF[i]) != b) error("expected header "b" at "i": "ELF[i])
    }
    # Make sure everything else matches our expectations:
    if (ELF[0x04] != 1) error("expected 32-bits executable")
    if (ELF[0x05] != 1) error("expected little endian executable")
    if (ELF[0x06] != 1) error("expected version 1 ELF")
    if (ELF[0x07] != 0) error("expected NONE at OSABI, got: " ELF[0x07])
    if (read_u16(ELF, 0x10) != 2) error("expected executable at 0x10")
    if (read_u16(ELF, 0x12) != 8) error("mips?, got: "read_u16(ELF, 0x12))
    if (read_u16(ELF, 0x14) != 1) error("v=1?, got: "read_u16(ELF, 0x12))
    ELF["entry"] = read_u32(ELF, 0x18)
    # printf "entry: 0x%x\n", ELF["entry"]
    ELF["phoff"] = read_u32(ELF, 0x1c)
    ELF["shoff"] = read_u32(ELF, 0x20)
    b = read_u32(ELF, 0x24)
    if (band(b, 1) != 1) error("expected .noreorder, flags="b)
    # having PIC or CPIC is also allowed (and expected)
    if (band(b, 0xf000) != 0x1000)
        error("expected o32 abi, got flags=0x" sprintf("%x", b))
    if (band(b, 0xff0000))
        error("EF_MIPS_MACH 0 expected, flags=0x", sprintf("%x", b))
    if (read_u16(ELF, 0x28) != 52) error("unexpected ehsize")
    if (read_u16(ELF, 0x2a) != 32) error("unexpected phentsize")
    ELF["phnum"] = read_u16(ELF, 0x2c)
    if (ELF["shnum"] && read_u16(ELF, 0x2e) != 40) error("weird shentsize")
    ELF["shnum"] = read_u16(ELF, 0x30)
    ELF["shstrndx"] = read_u16(ELF, 0x32)  # usually the last section (shnum-1)
}

function load_elf_program(ELF, MEM, CPU,
    phoff, i, type, offset, vaddr, filesz) {
    for (i = 0; i < 32; i++)
        CPU[i] = 0
    CPU["HI"] = CPU["LO"] = 0
    CPU["PC"] = ELF["entry"]
    for (phoff=ELF["phoff"]; phoff < ELF["phoff"]+ELF["phnum"]*32; phoff+=32) {
        type = read_u32(ELF, phoff)
        offset = read_u32(ELF, phoff + 4)
        vaddr = read_u32(ELF, phoff + 8)
        filesz = read_u32(ELF, phoff + 16)
        if (type == 0) {
            # PT_NULL:
        } else if (type == 1) {
            # PT_LOAD:
            for (i = 0; i < filesz; i++) MEM[vaddr + i] = ELF[offset + i]
            #printf "loaded %d bytes at 0x%x to 0x%x\n", filesz, offset, vaddr
        } else if (type == 0x70000000) {
            # PT_MIPS_REGINFO
            #CPU[REG_GP] = read_u32(ELF, offset + 0x4 * 5)
            #printf "$gp=0x%x\n", CPU[REG_GP]
        } else if (type == 0x70000003) {
            # PT_MIPS_ABIFLAGS (ignored)
        } else if (type == 0x6474e551) {
            # PT_GNU_STACK: for stack NX (ignored)
        } else {
            printf "unknown program header at phoff=0x%d: " \
                "type=0x%x, offset=0x%x, vaddr=0x%x, filesz=%d\n", \
                phoff, type, offset, vaddr, filesz
        }
    }
}

function load_stack(MEM, CPU, hex, args,
    ARGS, sp, OFFSETS, i, p, k, n) {
    sub(/\.hex$/, "", hex)
    argc = split(args, ARGS, /\|/) + 1
    ARGS[0] = hex

    # let's place the actual strings in GENERAL_RAM and move it upwards
    sp = INITIAL_SP
    OFFSETS[n++] = argc
    for (i = 0; i < argc; i++) {
        OFFSETS[n++] = GENERAL_RAM
        GENERAL_RAM = write_zstr(MEM, GENERAL_RAM, ARGS[i])
    }
    OFFSETS[n++] = 0  # a nice NULL pointer

    for (k in ENVIRON) {
        OFFSETS[n++] = GENERAL_RAM
        GENERAL_RAM = write_zstr(MEM, GENERAL_RAM, k "=" ENVIRON[k])
    }
    OFFSETS[n++] = 0

    # Now let's push the pointers to the stack:
    CPU[REG_SP] = INITIAL_SP - n*4
    for (i = 0; i < n; i++) {
        write_u32(MEM, CPU[REG_SP] + i*4, OFFSETS[i])
    }
}

function read_u32(BYTES, p) {
    # reads a 32bit unsigned little-endian int at position p in byte array BYTES
    #printf "  reading from %x\n", p
    return BYTES[p] + 256*(BYTES[p+1] + 256*(BYTES[p+2] + 256*BYTES[p+3]))
}

function write_u32(BYTES, p, value,
    i) {
    value = to_u32(value)
    if (DEBUG["write_u32"]) printf "  MEM[0x%08x] <- 0x%08x\n", p, value
    for (i = 0; i < 4; i++) {
        BYTES[p++] = value % 256
        value = int(value / 256)
    }
}

function write_zstr(BYTES, p, text,
    i, c) {
    for (i = 0; i < length(text); i++)
        BYTES[p++] = ORD[substr(text, i, 1)]
    BYTES[p++] = 0
    return p
}

function read_u16(BYTES, p) {
    # reads a 16bit unsigned little-endian int at position p in byte array BYTES
    return BYTES[p] + 256*BYTES[p+1]
}

function to_s8(value) {
    value %= 0x100  # range after this: [-(2^8-1), 2^8-1]
    if (value >= 0x80) value -= 0x100  # -> [-(2^8-1), 2^7-1]
    if (value < -0x80) value += 0x100  # -> [-(2^7), 2^7-1]
    return value
}

function to_s16(value) {
    value %= 0x10000  # range after this: [-(2^16-1), 2^16-1]
    if (value >= 0x8000) value -= 0x10000  # -> [-(2^16-1), 2^15-1]
    if (value < -0x8000) value += 0x10000  # -> [-(2^15), 2^15-1]
    return value
}

function to_u16(value) {
    value %= 0x10000  # range after this: [-(2^16-1), 2^16-1]
    return value < 0 ? value + 0x10000 : value  # -> [0, 2^16-1]
}

function to_s32(value) {
    value %= 0x100000000  # range after this: [-(2^32-1), 2^32-1]
    if (value >= 0x80000000) value -= 0x100000000  # -> [-(2^32-1), 2^31-1]
    if (value < -0x80000000) value += 0x100000000  # -> [-(2^31), 2^31-1]
    return value
}

function to_u32(value) {
    value %= 0x100000000  # range after this: [-(2^32-1), 2^32-1]
    return value < 0 ? value + 0x100000000 : value  # -> [0, 2^32-1]
}

function run_emulator_instruction(MEM, CPU,
    instr, pc, op, rs, rt, imm, tmp, lo1, lo2, hi1, hi2) {
    pc = CPU["PC"]
    # We use NEXTPC to simulate the branch-delay-slot
    if (CPU["NEXTPC"]) {
        CPU["PC"] = CPU["NEXTPC"]
        delete CPU["NEXTPC"]
    } else {
        CPU["PC"] = pc + 4
    }
    # Just in case section:
    CPU[0] = 0
    for (rs = 1; rs < 32; rs++) {
        tmp = CPU[rs]
        if(tmp<0) error("negative register found, check previous instruction!")
        if(tmp>0xffffffff) error("reg > 2^32-1 found, check previous instruction!")
        if(tmp!=int(tmp)) error("register with float, check previous instruction!")
    }
    instr = read_u32(MEM, pc)
    op = brshift(instr, 26)
    funct = instr % 0x40
    if (DEBUG["regs"])
        printf "  v0=%08x v1=%08x a0=%08x a1=%08x a2=%08x s2=%08x t9=%08x gp=%08x\n", \
               CPU[REG_V0], CPU[REG_V1], CPU[REG_A0], CPU[REG_A1], CPU[REG_A2], CPU[REG_S2], CPU[REG_T9], CPU[REG_GP]
    #for (tmp=0x412000; tmp<0x412030; tmp+=4) printf "  %x: %08x\n", tmp, read_u32(MEM, tmp)
    if (DEBUG["instr"]) printf "PC=%06x (%d): instr=0x%08x, op=0x%02x, funct=0x%02x\n", pc, CPU["CYCLES"], instr, op, funct
    rs = brshift(instr, 21) % 0x20
    rt = brshift(instr, 16) % 0x20
    rd = brshift(instr, 11) % 0x20
    imm = instr % 0x10000

    switch (op) {
    case 0: # SPECIAL instructions: (op=0)
        switch (funct) {
        case 0x0: # SLL rd, rt, sa
            tmp = brshift(instr, 6) % 0x20
            CPU[rd] = blshift(CPU[rt], tmp)
            break
        case 0x2: # SRL rd, rt, sa
            tmp = brshift(instr, 6) % 0x20
            CPU[rd] = brshift(CPU[rt], tmp)
            break
        case 0x3: # SRA rd, rt, sa
            tmp = brshift(instr, 6) % 0x20
            CPU[rd] = barshift(CPU[rt], tmp)
            break
        case 0x5: # LSA rd, rs, rt, sa
            # GPR[rd] := sign_extend.32( (GPR[rs] << (sa+1)) + GPR[rt] )
            tmp = brshift(instr, 6) % 0x4
            CPU[rd] = to_u32(lshift(CPU[rs], tmp) + CPU[rt])
            break
        case 0x8: # JR rs
            CPU["NEXTPC"] = CPU[rs]
            break
        case 0x9: # JALR [rd = 31 implied,] rs
            CPU[rd] = pc + 8
            CPU["NEXTPC"] = CPU[rs]
            break
        case 0xa: # MOVZ rd, rs, rt
            if (!CPU[rt]) CPU[rd] = CPU[rs]
            break
        case 0xc: # SYSCALL code
            CPU[REG_V0] = syscall(MEM, CPU, CPU[REG_V0], CPU[4], CPU[5], CPU[6], CPU[7])
            break
        case 0x12: # MFLO rd
            CPU[rd] = CPU["LO"]
            break
        case 0x1b: # DIVU rd, rs, rt
            if (rd)
                CPU[rd] = int(CPU[rs] / CPU[rt])
            else {
                CPU["LO"] = int(CPU[rs] / CPU[rt])
                CPU["HI"] = CPU[rs] % CPU[rt]
            }
            break
        case 0x21: # ADDU rd, rs, rt
            CPU[rd] = to_u32(CPU[rs] + CPU[rt])
            break
        case 0x23: # SUBU rd, rs, rt
            CPU[rd] = to_u32(CPU[rs] - CPU[rt])
            break
        case 0x24: # AND rd, rs, rt
            CPU[rd] = band(CPU[rs], CPU[rt])
            break
        case 0x25: # OR rd, rs, rt
            CPU[rd] = bor(CPU[rs], CPU[rt])
            break
        case 0x26: # XOR rd, rs, rt
            CPU[rd] = CPU[rs] + CPU[rt] - 2*band(CPU[rs], CPU[rt])
            break
        case 0x27: # NOR rd, rs, rt
            CPU[rd] = 0xffffffff - bor(CPU[rs], CPU[rt])
            break
        case 0x2b: # SLTU rd, rs, rt
            CPU[rd] = (CPU[rs] < CPU[rt]) ? 1 : 0
            break
        case 0x34: # TEQ rs, rt, code
            if (CPU[rs] == CPU[rt])
                error(sprintf("trap with code 0x%x at 0x%x", int(imm/0x40), pc))
            break
        default:
            error(sprintf("unknown special instruction, funct=0x%x", funct))
        }
        break

    case 1: # REGIMM instructions: (op=1)(rs)(rt)(immediate)
        switch (rt) {
        case 0x0: # BLTZ rs, offset
            if (CPU[rs] >= 0x80000000)
                CPU["NEXTPC"] = pc + 4 + to_s16(imm) * 4
            break
        case 0x1: # BGEZ rs, offset
            if (CPU[rs] < 0x80000000)
                CPU["NEXTPC"] = pc + 4 + to_s16(imm) * 4
            break
        case 0x11: # BGEZAL (BAL when rs=0, deprecated otherwise)
            if (CPU[rs] < 0x80000000) {
                CPU[REG_RA] = pc + 8
                CPU["NEXTPC"] = pc + 4 + to_s16(imm) * 4
            }
            break
        default:
            error(sprintf("unknown regimm instruction, rt=0x%x", rt))
        }
        break

    # Regular Instructions:
    case 0x2: # J instr_index
        CPU["NEXTPC"] = int((pc+4)/0x10000000)*0x10000000 + instr%0x4000000*4
        break
    case 0x3: # JAL instr_index
        CPU[REG_RA] = pc + 8
        CPU["NEXTPC"] = int((pc+4)/0x10000000)*0x10000000 + instr%0x4000000*4
        break
    case 0x4: # BEQ rs, rt, imm
        if (CPU[rs] == CPU[rt])
            CPU["NEXTPC"] = pc + 4 + to_s16(imm) * 4
        break
    case 0x5: # BNE rs, rt, imm
        if (CPU[rs] != CPU[rt])
            CPU["NEXTPC"] = pc + 4 + to_s16(imm) * 4
        break
    case 0x9: # ADDIU rt, rs, imm
        CPU[rt] = to_u32(CPU[rs] + to_s16(imm))
        break
    case 0xb: # SLTIU rt, rs, imm
        CPU[rt] = (CPU[rs] < to_u32(to_s16(imm))) ? 1 : 0
        break
    case 0xc: # ANDI rt, rs, imm
        CPU[rt] = band(CPU[rs], imm)
        break
    case 0xd: # ORI rt, rs, imm
        CPU[rt] = bor(CPU[rs], imm)
        break
    case 0xf: # LUI rt, imm
        CPU[rt] = imm * 0x10000
        break

    case 0x1c: # SPECIAL2 Instructions
        switch (funct) {
        case 0x2: # MUL rd, rs, rt
            lo1 = CPU[rs] % 0x10000
            lo2 = CPU[rt] % 0x10000
            hi1 = int(CPU[rs] / 0x10000)
            hi2 = int(CPU[rt] / 0x10000)
            CPU[rd] = to_u32(lo1*lo2 + (lo1*hi2 + lo2*hi1)*0x10000)
            break
        default:
            error(sprintf("unknown SPECIAL2 instruction 0x%x", funct))
        }
        break

    case 0x1f: # SPECIAL3 Instructions
        switch (funct) {
        case 0x20: # BSHFL:
            tmp = brshift(instr, 6) % 0x20
            switch (tmp) {
            case 0x10: # SEB rd, rt
                CPU[rd] = to_s8(CPU[rt])
                break
            default:
                error(sprintf("unknown BSHFL instruction 0x%x", tmp))
            }
            break
        case 0x3b: # RDHWR rt, rd[, sel]
            if (rd != 29)
                error("RDHWR only supports reading hw register $29")
            CPU[rt] = CPU["TLS_TP"]
            break
        default:
            error(sprintf("unknown SPECIAL3 instruction 0x%x", funct))
        }
        break
    case 0x20: # LB rt, imm(rs)
        CPU[rt] = to_s8(MEM[CPU[rs] + to_s16(imm)])
        break
    case 0x23: # LW rt, imm(rs)
        CPU[rt] = read_u32(MEM, CPU[rs] + to_s16(imm))
        break
    case 0x24: # LBU rt, imm(rs)
        CPU[rt] = MEM[CPU[rs] + to_s16(imm)]
        break
    case 0x28: # SB rt, imm(rs)
        MEM[CPU[rs] + to_s16(imm)] = CPU[rt] % 256
        break
    case 0x2b: # SW rt, imm(rs)
        write_u32(MEM, CPU[rs] + to_s16(imm), CPU[rt])
        break
    default:
        error("unknown regular instruction")
    }
    CPU["CYCLES"]++
}

function syscall(MEM, CPU, nr, a0, a1, a2, a3,
    lo, fd, s, i, base, len, j) {
    # Called when a SYSCALL instruction is issued.
    if (DEBUG["syscall"]) printf "syscall(%d, 0x%x, 0x%x, 0x%x, 0x%x)\n", nr, a0, a1, a2, a3
    switch (nr - 4000) {
    case 54: # int ioctl(int fd, unsigned long request, ...);
        # uppermost 2 bits of a0 contain DIR (_IO=0, _IOW=1, _IOR=2, _IOWR=3)
        # then 14 bits for the size of the structure.
        # then 8 bits for the type
        # and finally the lower 8 bits for the number
        switch (a0) {
            case 0x40087468: # TIOCGWINSZ=_IOR('t', 104, struct winsize): get window size
                # man ioctl_tty
                write_u32(MEM, a1, 80 * 0x10000 + 24)  # lie saying it's 80x24
                write_u32(MEM, a1+4, 0)  # we don't even know the size
                break
        }
        return 0
    case 146: # ssize_t writev(int fd, const struct iovec *iov, int iovcnt);
        fd = a0==1?"/dev/stdout" : a0==2?"/dev/stderr" : ""
        if (!fd) return -9  #-EBADF
        for (i = 0; i < a2; i++) {
            # a1 is an array of struct iovec {void* iov_base; size_t iov_len;};
            base = read_u32(MEM, a1 + i*8)
            len = read_u32(MEM, a1 + i*8 + 4)
            for (j = 0; j < len; j++) s = s sprintf("%c", MEM[base + j])
            if (DEBUG["writev"]) printf "write base: 0x%x, len: %d\n", base, len
        }
        printf "%s", s >fd
        return length(s)
    case 246: # void exit(int status);
        exit a0
    case 252: # pid_t set_tid_address(int *tidptr);
        # store tidptr somewhere... I guess
        return 42  # the thread_id
    case 283: # int set_thread_area(unsigned long addr):
        CPU["TLS_TP"] = a0
        return 0
    default:
        error("unknown syscall! use -vdebug=syscall")
    }
}

function run_emulator(MEM, CPU) {
    for (;;) run_emulator_instruction(MEM, CPU)
}

function main(hex, args,
    ELF, MEM, CPU, i, A) {
    if (!hex) error("hex is undefined, run emulator with -vhex=filename.hex")

    # Load ORD (array inverse to λc.sprintf("%c", c))
    for (i = 1; i < 256; i++) ORD[sprintf("%c", i)] = i

    # load DEBUG from debug
    A["n"] = split(debug, A, ",")
    for (i = 1; i <= A["n"]; i++) DEBUG[A[i]]++

    load_elf_file(hex, ELF)
    load_elf_program(ELF, MEM, CPU)
    load_stack(MEM, CPU, hex, args)
    run_emulator(MEM, CPU)
}
BEGIN {main(hex, args)}
