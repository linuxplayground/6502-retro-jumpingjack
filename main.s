; vim: set ft=asm_ca65 ts=4 sw=4 et:
.include "io.inc"
.include "app.inc"

NUM_GAPS = 8
DEBUG = 0

.autoimport
.globalzp ptr1, ptr2, tmp1

.zeropage
ptr1:   .word 0
ptr2:   .word 0
tmp1:   .byte 0
tmp2:   .byte 0
frame:  .byte 0

.bss
line:   .byte 0
gap:    .byte 0

.code

start:
    jsr vdp_g2_init
.if DEBUG
    lda #1
    ldx #$11
    jsr vdp_color_char

    lda #2
    ldx #$21
    jsr vdp_color_char

    lda #3
    ldx #$31
    jsr vdp_color_char

    lda #4
    ldx #$41
    jsr vdp_color_char

    lda #6
    ldx #$51
    jsr vdp_color_char

    lda #7
    ldx #$61
    jsr vdp_color_char

    lda #8
    ldx #$71
    jsr vdp_color_char
.endif
    jsr draw_lines
game_loop:
    ; frame 1/4
    jsr draw_gaps
    jsr vdp_wait
    jsr vdp_flush
    inc frame
.if DEBUG
    jsr debug_pause
.endif
    ; frame 2/4
    jsr draw_gaps
    jsr vdp_wait
    jsr vdp_flush
    inc frame
.if DEBUG
    jsr debug_pause
.endif
    ; frame 3/4
    jsr draw_gaps
    jsr vdp_wait
    jsr vdp_flush
    inc frame
.if DEBUG
    jsr debug_pause
.endif
    ; frame 4/4
    jsr draw_gaps
    jsr vdp_wait
    jsr vdp_flush
    inc frame
.if DEBUG
    jsr debug_pause
.endif

    inc gaps_pos+0
    inc gaps_pos+1
    inc gaps_pos+2
    inc gaps_pos+3
    dec gaps_pos+4
    dec gaps_pos+5
    dec gaps_pos+6
    dec gaps_pos+7


    jsr CONST
    cmp #$1b
    beq exit
    jmp game_loop
exit:
    jmp WBOOT

debug_pause:
    ldx #1
:   jsr vdp_wait
    dex
    bne :-
    rts

draw_lines:
    ldy #0
    jsr draw_line
    ldy #3
    jsr draw_line
    ldy #6
    jsr draw_line
    ldy #9
    jsr draw_line
    ldy #12
    jsr draw_line
    ldy #15
    jsr draw_line
    ldy #18
    jsr draw_line
    ldy #21
    ; fall through
draw_line:
    ldx #0
    jsr vdp_xy_to_ptr
    ldy #31
    lda #1
:   sta (ptr1),y
    dey
    bpl :-
    rts

get_gap_xy:
    pha
    lsr
    lsr
    lsr
    lsr
    lsr
    sta tmp1
    asl
    clc
    adc tmp1
    tay
    pla
    and #$1F
    tax
    rts

draw_gaps:
    lda frame
    and #3
    asl
    asl         ; multiply by 4
    sta gaps_frame_jump + 1 ; self modifying code

gaps_frame_jump:
    bra gaps_frame_jump
    jmp gaps_F0      ; +0 (Frame 0)
    nop
    jmp gaps_F1      ; +4 (Frame 1)
    nop
    jmp gaps_F2      ; +8 (Frame 2)
    nop
    jmp gaps_F3      ; +12 (Frame 3)

gaps_F0:
.if DEBUG
    lda #'0'
    jsr CONOUT
.endif
    lda #(NUM_GAPS-1)
    sta gap
@gaploop:
    ; draw the outsides
    ldx gap
    lda gaps_pos,x
    dec
    jsr get_gap_xy      ; GAP 0
    lda #1
    jsr vdp_char_xy

    ldx gap
    lda gaps_pos,x
    clc
    adc #3
    jsr get_gap_xy      ; GAP 4
    lda #1
    jsr vdp_char_xy

    ; draw the middle gaps
    ldx gap
    lda gaps_pos,x
    pha
    jsr get_gap_xy      ; GAP 1
    lda #5
    jsr vdp_char_xy
    pla
    inc
    pha
    jsr get_gap_xy      ; GAP 2
    lda #5
    jsr vdp_char_xy
    pla
    inc
    jsr get_gap_xy      ; GAP 3
    lda #5
    jsr vdp_char_xy

    ; gaploop
    dec gap
    bpl @gaploop
    rts

gaps_F1:
.if DEBUG
    lda #'1'
    jsr CONOUT
.endif
    lda #4
    sta gap_frame_data + 0  ;11000000b      ; Cell 1 (Right-moving gaps)
    lda #6
    sta gap_frame_data + 1  ;00000011b      ; Cell 3 (Left-moving gaps)
    lda #8
    sta gap_frame_data + 2  ;00111111b      ; Cell 4 (Right-moving gaps)
    lda #2
    sta gap_frame_data + 3  ;11111100b      ; Cell 0 (Left-moving gaps)

    jmp gaps_F123

gaps_F2:
.if DEBUG
    lda #'2'
    jsr CONOUT
.endif
    lda #3
    sta gap_frame_data + 0  ;11110000b      ; Cell 1 (Right-moving gaps)
    lda #7
    sta gap_frame_data + 1  ;00001111b      ; Cell 3 (Left-moving gaps)
    lda #7
    sta gap_frame_data + 2  ;00001111b      ; Cell 4 (Right-moving gaps)
    lda #3
    sta gap_frame_data + 3  ;11110000b      ; Cell 0 (Left-moving gaps)
    jmp gaps_F123

gaps_F3:
.if DEBUG
    lda #'3'
    jsr CONOUT
.endif
    lda #2
    sta gap_frame_data + 0  ;11111100b      ; Cell 1 (Right-moving gaps)
    lda #8
    sta gap_frame_data + 1  ;00111111b      ; Cell 3 (Left-moving gaps)
    lda #6
    sta gap_frame_data + 2  ;00000011b      ; Cell 4 (Right-moving gaps)
    lda #4
    sta gap_frame_data + 3  ;11000000b      ; Cell 0 (Left-moving gaps)

    ; fall through
gaps_F123:
    ; draw cell 1 in gaps 0-3 (right down gaps)
    lda #3
    sta gap
@right_moving_gaps_cell_1:
    ldx gap
    lda gaps_pos,x
    jsr get_gap_xy
    lda gap_frame_data+0
    jsr vdp_char_xy
    dec gap
    bpl @right_moving_gaps_cell_1

    ; Draw Cell 3 in gaps 4-7 (the Left/Up gaps)
    lda #7
    sta gap
@left_moving_gaps_cell_3:
    ldx gap
    lda gaps_pos,x
    inc
    inc
    jsr get_gap_xy
    lda gap_frame_data+1
    jsr vdp_char_xy
    dec gap
    lda gap
    cmp #3
    bne @left_moving_gaps_cell_3

    ; Draw gaps 0-3 (the Right/Down gaps)
    ; For each, AND the desired contents of cells 1 & 4 with what's
    ; already on the screen to allow for overlapping left-moving
    ; gaps.
    lda #3
    sta gap
@right_moving_gaps_14:
    lda gap_frame_data+0
    sta tmp2
    ldx gap
    lda gaps_pos,x          ; point to cell 1
    jsr gap_and_update

    lda gap_frame_data+2
    sta tmp2
    ldx gap
    lda gaps_pos,x
    clc
    adc #3                  ; point to cell 4
    jsr gap_and_update

    ldx gap
    lda gaps_pos,x
    inc                     ; point to cell 2
    jsr get_gap_xy
    lda #5                  ; empty gap
    jsr vdp_char_xy

    ldx gap
    lda gaps_pos,x
    inc
    inc                     ; point to cell 3
    jsr get_gap_xy
    lda #5                  ; empty gap
    jsr vdp_char_xy

    dec gap
    bpl @right_moving_gaps_14

    ; Draw gaps 4-7 (the Left/Up gaps)
    ; For each, AND the desired contents of cells 0 & 3 with what's
    ; already on the screen to allow for overlapping right-moving
    ; gaps.
    lda #7
    sta gap
@left_moving_gaps_03:
    lda gap_frame_data+1
    sta tmp2
    ldx gap
    lda gaps_pos,x
    inc
    inc                     ; point to cell 3
    jsr gap_and_update

    lda gap_frame_data+3
    sta tmp2
    ldx gap
    lda gaps_pos,x
    dec                     ; point to cell 0
    jsr gap_and_update

    ldx gap
    lda gaps_pos,x          ; point to cell 1
    jsr get_gap_xy
    lda #5                  ; empty gap
    jsr vdp_char_xy

    ldx gap
    lda gaps_pos,x
    inc                     ; point to cell 2
    jsr get_gap_xy
    lda #5                  ; empty gap
    jsr vdp_char_xy

    dec gap
    lda gap
    cmp #3
    bne @left_moving_gaps_03
    rts

; INPUT: A Cell position
;        tmp2 desired pattern
gap_and_update:
    pha         ; save cell position
    jsr get_gap_xy
    jsr vdp_read_char_xy
    asl         ; x 2
    tax
    lda gap_and_idx+0,x
    sta ptr2 + 0
    lda gap_and_idx+1,x
    sta ptr2 + 1
    ldy tmp2    ; desired pattern in Y
    dey
    lda (ptr2),y
    sta tmp2    ; save new pattern
    pla
    jsr get_gap_xy
    lda tmp2
    jsr vdp_char_xy
    rts


.segment "DATA"

gaps_pos:
    .byte $00   ; right down
    .byte $22   ; right down
    .byte $44   ; right down
    .byte $66   ; right down

    .byte $88   ; left up
    .byte $aa   ; left up
    .byte $cc   ; left up
    .byte $ee   ; left up

gap_frame_data:
    .byte 0     ; Cell 1 (Right-moving gaps)
    .byte 0     ; Cell 3 (Left-moving gaps)
    .byte 0     ; Cell 4 (Right-moving gaps)
    .byte 0     ; Cell 0 (Left-moving gaps)

.rodata

gap_and_idx:
    .word 0
    .addr gap_ones
    .addr gap_twos
    .addr gap_threes
    .addr gap_fours
    .addr gap_fives
    .addr gap_sixes
    .addr gap_sevens
    .addr gap_eights
    .addr gap_nines
    .addr gap_tens

gap_ones:
    .byte 1,2,3,4,5,6,7,8
gap_twos:
    .byte 2,2,3,4,5,5,9,10
gap_threes:
    .byte 3,3,3,4,5,6,5,9
gap_fours:
    .byte 4,4,4,4,5,10,4,5
gap_fives:
    .byte 5,5,5,5,5,5,5,5
gap_sixes:
    .byte 6,5,6,10,5,6,6,6
gap_sevens:
    .byte 7,9,5,4,5,6,7,7
gap_eights:
    .byte 8,10,9,5,5,6,7,8
gap_nines:
    .byte 5,5,5,5,5,5,5,5
gap_tens:
    .byte 5,5,5,5,5,5,5,5

