define EN_INCR = 1
define FOO_BAR = 2

instruction JIZ = 34

enable FOO_BAR EN_INCR DIS_ALU EN_REG_A

if carry {
    enable FOO_BAR EN_INCR DIS_ALU EN_REG_A
    enable FOO_X FOO_Y
}

step
