; vim: set ft=asm_ca65 et ts=4 sw=4
sfos_entry      = $200
bios_cboot      = $203
bios_wboot      = $206
bios_conout     = $209
bios_conin      = $20c
bios_const      = $20f
bios_puts       = $212
bios_prbyte     = $215

bios_setdma     = $218
bios_setlba     = $21b
bios_sdread     = $21e
bios_sdwrite    = $221

bios_sn_beep    = $224
bios_sn_start   = $227
bios_sn_silence = $22a
bios_sn_stop    = $22d
bios_sn_send    = $230
bios_led_on     = $233
bios_led_off    = $236
bios_get_button = $239

bios_error_code = $23c

bios_rstfar     = $23d

bios_rega       = $24d
bios_regx       = bios_rega   + 1
bios_regy       = bios_regx   + 1

bios_userirq_vec = $250
bios_usernmi_vec = $252

bios_ticks      = $65F    ; 32bit value holding number of 60hz periods since boot
;
; hard coded addresses that SFCP uses
FCB          = $380
FCB2         = $3A0
CMDLINE      = $300
CMDOFFSET    = $3C0    ; pointer into CMDLINE
SFOS_BUF     = $400
SFOS_BUF_END = $600

VDP_SYNC     = $65E
TICKS        = bios_ticks

; vdp stuffs
NAMETABLE               = $3800
COLORTABLE              = $2000
PATTERNTABLE            = $0000
SPRITEATTRIBUTETABLE    = $3B00
SPRITEPATTERNTABLE      = $1800

.enum jstate
    still   = 0
    left    = 1
    right   = 2
    jump_1  = 3 ; 
    jump_2  = 4 ; good jump
    jump_3  = 5 ; good jump
    falling = 6
    crash   = 7 ; when a jump is bad
    crash_fall = 8
    stun    = 9 ; second half of a bad jump
.endenum

.struct sprite
    yp .byte
    xp .byte
    pa .byte
    co .byte
.endstruct
