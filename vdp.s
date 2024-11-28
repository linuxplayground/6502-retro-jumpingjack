; vim: set ft=asm_ca65 ts=4 sw=4 et:vdp
.include "io.inc"
.include "app.inc"
.include "macro.inc"

.export vdp_g2_init, vdp_clear_screenbuf, vdp_wait, vdp_flush
.export vdp_screenbuf, vdp_xy_to_ptr, vdp_print_xy, vdp_char_xy
.export vdp_read_char_xy, vdp_color_char

.autoimport

.globalzp ptr1, ptr2

.zeropage

.bss
.align $100
vdp_screenbuf: .res $300

.code

vdp_xy_to_ptr:
    pha
    lda #<vdp_screenbuf
    sta ptr1
    lda #>vdp_screenbuf
    sta ptr1+1

    tya
    div8
    clc
    adc ptr1+1
    sta ptr1+1
    tya
    and  #$07
    mul32
    sta ptr1
@add_x:
    clc
    txa
    adc ptr1
    sta ptr1
    lda #0
    adc ptr1+1
    sta ptr1+1
@return:
    pla
    rts

vdp_char_xy:
    jsr vdp_xy_to_ptr
    sta (ptr1)
    rts

vdp_read_char_xy:
    jsr vdp_xy_to_ptr
    lda (ptr1)
    rts

; string pointer in ptr2
vdp_print_xy:
    jsr vdp_xy_to_ptr
    ldy #0
:   lda (ptr2),y
    beq :+
    sta (ptr1),y
    iny
    bra :-
:   rts

vdp_g2_init:
    jsr clear_vram
    lda #<g2_regs
    ldx #>g2_regs
    jsr init_regs
    lda #$6e
    jsr setup_colortable
    jsr load_font
    jsr vdp_clear_screenbuf
    rts

clear_vram:
    lda #0
    ldx #0
    jsr set_write_address
    lda #0
    ldy #0
    ldx #$3F
:   sta vdp_ram
    iny
    bne :-
    dex
    bne :-
    rts

; INPUT: A = character
;        X = color
vdp_color_char:
    phx
    asl
    asl
    asl     ; x 8
    sta ptr1+0
    lda #<COLORTABLE
    clc
    adc ptr1+0
    sta ptr1+0
    lda #>COLORTABLE
    adc #0
    sta ptr1+1
    lda ptr1+0
    ldx ptr1+1
    jsr set_write_address
    plx
    .repeat 8
        stx vdp_ram
    .endrepeat
    rts


vdp_wait:
    lda VDP_SYNC
    cmp #$80
    bne vdp_wait
    stz VDP_SYNC
.if 0
    jsr CONIN
    cmp #$1b
    beq @exit
.endif
    rts
@exit:
    jmp WBOOT

vdp_flush:
    lda #<NAMETABLE
    ldx #>NAMETABLE
    jsr set_write_address
    lda #<vdp_screenbuf
    sta ptr1
    lda #>vdp_screenbuf
    sta ptr1 + 1
    ldy #0
    ldx #4
:   lda (ptr1),y
    sta vdp_ram
    iny
    bne :-
    inc ptr1+1
    dex
    bne :-
    rts

vdp_clear_screenbuf:
    lda #<vdp_screenbuf
    sta ptr1
    lda #>vdp_screenbuf
    sta ptr1 + 1
    ldy #0
    ldx #4
    lda #' '
:   sta (ptr1),y
    iny
    bne :-
    inc ptr1+1
    dex
    bne :-
    rts

setup_colortable:
    tay
    lda #<COLORTABLE
    ldx #>COLORTABLE
    jsr set_write_address
    tya
    ldy #0
    ldx #4
:   sta vdp_ram
    iny
    bne :-
    dex
    bne :-
    rts

init_regs:
    sta ptr1
    stx ptr1+1
    ldy #0
:   lda (ptr1),y
    sta vdp_reg
    tya
    ora #$80
    sta vdp_reg
    iny
    cpy #8
    bne :-
    rts

set_write_address:
    sta vdp_reg
    txa
    ora #$40
    sta vdp_reg
    rts

load_font:
    lda #<PATTERNTABLE
    ldx #>PATTERNTABLE
    jsr set_write_address
    lda #<font_start
    sta ptr1
    lda #>font_start
    sta ptr1+1
    ldy #0
:   lda (ptr1),y
    sta vdp_ram
    lda ptr1
    clc
    adc #1
    sta ptr1
    lda #0
    adc ptr1+1
    sta ptr1+1
    cmp #>font_end
    bne :-
    lda ptr1
    cmp #<font_end
    bne :-
    rts

.rodata
g2_regs:
    .byte $02
    .byte $e0
    .byte $0e
    .byte $9f
    .byte $00
    .byte $76
    .byte $03
    .byte $2b

font_start:
    .include "font.s"
font_end:
