; vim: set ft=asm_ca65 ts=4 sw=4 et:
.include "io.inc"
.include "app.inc"
.include "macro.inc"

NUM_GAPS = 8

.enum JACK_STATE
    IDLE        = 1 ;standing still
    RIGHT       = 2 ;running right
    LEFT        = 3 ;running left
    JUMP        = 4 ;good jump
    JUMPCRASH   = 5 ;jump bad   applies to phase 2 of jump
    JUMPFALL    = 7 ;jump bad   applies to phase 3 of jump
    FALL        = 8
    STUNNED     = 9
.endenum

.enum JACK_ANIM
    IDLE    = 1
    RIGHT   = 2
    LEFT    = 3
    JUMP    = 4
    FALL    = 5
    STUN    = 6
    CRASH   = 7
.endenum

.autoimport
.globalzp ptr1, ptr2, tmp1

.zeropage
ptr1:   .word 0
ptr2:   .word 0
tmp1:   .byte 0
tmp2:   .byte 0
jprev:  .byte 0
frame:  .byte 0

.bss
line:   .byte 0
gap:    .byte 0
jline:  .byte 0
gaps_pos:
    .byte $00   ; right down
    .byte $00   ; right down
    .byte $00   ; right down
    .byte $00   ; right down

    .byte $00   ; left up
    .byte $00   ; left up
    .byte $00   ; left up
    .byte $00   ; left up

jack_state:      .byte 0
jack_anim:       .byte 0
jack_cur_frame:  .byte 0
jack_jump_frame: .byte 0
jack_jump_state: .byte 0
jack_jumping:    .byte 0
jack_falling:    .byte 0

gap_frame_data:
    .byte 0     ; Cell 1 (Right-moving gaps)
    .byte 0     ; Cell 3 (Left-moving gaps)
    .byte 0     ; Cell 4 (Right-moving gaps)
    .byte 0     ; Cell 0 (Left-moving gaps)

seed:   .byte 0 ; initial seed for random number gen.
gap_count: .byte 0

.code

start:
    jsr vdp_g2_init

    lda #<sprite_start
    sta ptr1
    lda #>sprite_start
    sta ptr1+1
    lda #<sprite_end
    sta ptr2
    lda #>sprite_end
    sta ptr2+1
    jsr load_sprite_patterns

    lda #<font_start
    sta ptr1
    lda #>font_start
    sta ptr1+1
    lda #<font_end
    sta ptr2
    lda #>font_end
    sta ptr2+1
    jsr load_font_patterns

    lda #(192-16)
    sta sprite0 + sprite::yp
    lda #128
    sta sprite0 + sprite::xp
    lda #4
    sta sprite0 + sprite::pa
    lda #1
    sta sprite0 + sprite::co
    jsr flush_sprite_attributes

    jsr reset_data

    stz frame
    stz tmp1
    stz tmp2
    stz gap
    stz line

    jsr draw_lines
seed_loop:
    inc seed
    jsr CONST
    bcc seed_loop

    jsr rnd
    sta gaps_pos+0
    sta gaps_pos+1
    sta gaps_pos+2
    sta gaps_pos+3

    jsr rnd
    sta gaps_pos+4
    sta gaps_pos+5
    sta gaps_pos+6
    sta gaps_pos+7

    lda #2
    sta gap_count

game_loop:
    ; frame 1/4
    jsr draw_gaps
    jsr move_jack
    inc frame

    jsr vdp_wait
    jsr vdp_flush
    jsr flush_sprite_attributes

    ; frame 2/4
    jsr draw_gaps
    inc frame
    jsr move_jack
    jsr vdp_wait
    jsr vdp_flush
    jsr flush_sprite_attributes

    ; frame 3/4
    jsr draw_gaps
    inc frame
    jsr move_jack
    jsr vdp_wait
    jsr vdp_flush
    jsr flush_sprite_attributes

    ; frame 4/4
    jsr draw_gaps
    inc frame

    inc gaps_pos+0
    inc gaps_pos+1
    inc gaps_pos+2
    inc gaps_pos+3
    dec gaps_pos+4
    dec gaps_pos+5
    dec gaps_pos+6
    dec gaps_pos+7

    jsr move_jack
    jsr animate_jack

    jsr vdp_wait
    jsr vdp_flush
    jsr flush_sprite_attributes

    lda jack_jumping
    beq :+
    jmp game_loop
:   cmp jack_falling
    beq :+
    jmp game_loop
:   jsr test_fall
; jack state before asking for user input.

@get_key:
    jsr CONST
    sta tmp1
    cmp #$1b
    bne @check_state
    jmp exit
@check_state:
    lda jack_jumping
    beq :+
    jmp game_loop
:   lda jack_falling
    beq @key_stop
    jmp game_loop
@key_stop:
    lda tmp1
    cmp #'s'
    bne @key_left
    lda #JACK_STATE::IDLE
    sta jack_state
    lda #JACK_ANIM::IDLE
    sta jack_anim
    jmp game_loop
@key_left:
    cmp #'a'
    bne @key_right
    lda #JACK_STATE::LEFT
    sta jack_state
    lda #JACK_ANIM::LEFT
    sta jack_anim
    jmp game_loop
@key_right:
    cmp #'d'
    bne @key_jump
    lda #JACK_STATE::RIGHT
    sta jack_state
    lda #JACK_ANIM::RIGHT
    sta jack_anim
    jmp game_loop
@key_jump:
    cmp #' '
    bne @no_key
    ldy jline
    dey
    bmi @no_key
    lda sprite0 + sprite::xp
    lsr
    lsr
    lsr
    tax
    jsr get_xy_gap  ; above jack
    sta jprev
    jsr test_gap
    bcs :+
    lda jprev
    dec             ; one to the left
    jsr test_gap
    bcs :+
    ; BAD JUMP
    lda #1
    sta jack_jump_state
    bra :++
    ;GOOD JUMP
:   stz jack_jump_state

:   lda JACK_STATE::JUMP
    sta jack_state
    lda #JACK_ANIM::JUMP
    sta jack_anim
    lda #$ff
    sta jack_jump_frame
    lda #1
    sta jack_jumping
    stz jack_cur_frame
@no_key:
    jmp game_loop
exit:
    jmp WBOOT

test_fall:
    ldy jline
    cpy #8
    bne :+
    rts
:   lda sprite0 + sprite::xp
    lsr
    lsr
    lsr
    tax
    jsr get_xy_gap  ; Jack's feet
    sta jprev
    jsr test_gap
    bcs :+
    lda jprev
    dec             ; one to the left
    jsr test_gap
    bcs :+
    ; not falling
    stz jack_falling
    lda #0
    rts
    ; falling
:
    lda JACK_STATE::FALL
    sta jack_state
    lda #JACK_ANIM::FALL
    sta jack_anim
    lda #$ff
    sta jack_jump_frame
    lda #1
    sta jack_falling
    stz jack_cur_frame
    rts

test_gap:
    ldx #7
@L0:
    cmp gaps_pos,x
    beq @match
    dex
    bpl @L0
    clc
    rts
@match:
    sec
    rts

animate_jack:
    lda jack_anim
    dec
    asl
    asl
    asl
    asl
    sta tmp1
    lda jack_cur_frame
    and #3
    inc
    asl
    asl
    clc
    adc tmp1
    sta sprite0 + sprite::pa
    inc jack_cur_frame
@exit:
    rts

move_jack:
    ; if jack is idle, do nothing.
    ; if jack is RIGHT, move right
    ; if jack is LEFT, move left
    lda jack_falling
    beq :+
    jmp check_fall
:   lda jack_state
    cmp #JACK_STATE::IDLE
    bne check_left
    rts
check_left:
    cmp #JACK_STATE::LEFT
    bne check_right
    dec sprite0 + sprite::xp
    dec sprite0 + sprite::xp
    rts
check_right:
    cmp #JACK_STATE::RIGHT
    bne check_jumping
    inc sprite0 + sprite::xp
    inc sprite0 + sprite::xp
    rts
check_jumping:
    lda jack_jumping
    beq check_fall
    jmp do_jump
check_fall:
    inc jack_jump_frame
    lda jack_jump_frame
    cmp #12
    bne :++
    lda jline
    cmp #8
    bcs :+
    inc jline
:   stz jack_falling
    lda jline
    jsr CONBYTE
    jmp reset_jump
:   inc sprite0+sprite::yp
    inc sprite0+sprite::yp
    rts

do_jump:
    inc jack_jump_frame
    lda jack_jump_frame
    cmp #4
    bcs @phase2    ; frame is = 4
    dec sprite0 + sprite::yp
    dec sprite0 + sprite::yp
    rts
@phase2:
    lda jack_jump_frame
    cmp #4
    bne :+
    lda jack_jump_state
    beq :+
    lda #JACK_ANIM::CRASH
    sta jack_anim
    rts
:   lda jack_jump_frame
    cmp #7
    bcs @phase3     ; frame is >= 7
    lda jack_jump_state
    bne :+
    dec sprite0 + sprite::yp    ; good jump - keep going up
    dec sprite0 + sprite::yp
:   rts
@phase3:
    lda jack_jump_frame
    cmp #7
    bne :+
    lda jack_jump_state
    beq :+
    lda #JACK_ANIM::FALL
    sta jack_anim
    rts
:   cmp #12
    bne :++
    lda jack_jump_state
    bne :+
    dec jline
:   jmp reset_jump      ; frame is 12
:   lda jack_jump_state
    bne :+
    dec sprite0 + sprite::yp    ; good jump - keep going up
    dec sprite0 + sprite::yp
    rts
:   inc sprite0 + sprite::yp    ; bad jump - fall back down
    inc sprite0 + sprite::yp
    rts

reset_jump:
    lda jline
    pha
    jsr CONBYTE
    pla
    beq win

    stz jack_jump_frame
    lda #JACK_STATE::IDLE
    sta jack_state
    lda #JACK_ANIM::IDLE
    sta jack_anim
    stz jack_jumping
    lda jack_jump_state
    bne :+
    jsr do_new_gap
:   rts
win:
    lda #<WINNER
    ldx #>WINNER
    jsr CONPUTS
    jmp exit

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

; Convert XY to gap location so it can be compared with all gaps
; INPUT X, Y location of sprite
; RETURN A = LLLXXXXX location of tile.
get_xy_gap:
    tya     ; y is a char position 0-23
    asl     ; < 1
    asl     ; < 2
    asl     ; < 3
    asl     ; < 4
    asl     ; shift into LLL position
    sta tmp1
    txa     ; x is char position 0-31
    ora tmp1
    rts

get_gap_xy:
    pha
    lsr
    lsr
    lsr
    lsr
    lsr     ; A >> 5
    sta tmp1
    asl     ; x 2
    clc
    adc tmp1 ; + 1 (Line number x 3)
    tay
    pla
    and #$1F ; X is okay as is.
    tax
    rts

do_new_gap:
    lda gap_count
    cmp #8
    beq :+
    inc gap_count
    ldy gap_count
    lda #<gaps_pos
    sta ptr1
    lda #>gaps_pos
    sta ptr1+1
    jsr rnd
    sta (ptr1),y
:
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
    jsr get_gap_xy      ; CELL 0
    lda #1
    jsr vdp_char_xy

    ldx gap
    lda gaps_pos,x
    clc
    adc #3
    jsr get_gap_xy      ; CELL 4
    lda #1
    jsr vdp_char_xy

    ; draw the middle gaps
    ldx gap
    lda gaps_pos,x
    pha
    jsr get_gap_xy      ; CELL 1
    lda #5
    jsr vdp_char_xy
    pla
    inc
    pha
    jsr get_gap_xy      ; CELL 2
    lda #5
    jsr vdp_char_xy
    pla
    inc
    jsr get_gap_xy      ; CELL 3
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

flush_sprite_attributes:
    lda #<SPRITEATTRIBUTETABLE
    ldx #>SPRITEATTRIBUTETABLE
    jsr set_write_address

    lda #<sprite0
    sta ptr1
    lda #>sprite0
    sta ptr1+1
    ldy #0
@L1:
    lda (ptr1),y
    cmp #$D0
    beq @EXIT
    sta vdp_ram
    iny
    bpl @L1
@EXIT:
    rts

reset_data:
    ldx #11
@L1:
    lda gaps_start_pos,x
    sta gaps_pos,x
    dex
    bpl @L1
    lda #8
    sta jline
    stz jack_jump_frame
    stz jack_jumping
    stz jack_falling
    rts

rnd:
     lda seed
     beq doEor
     asl
     beq noEor ;if the input was $80, skip the EOR
     bcc noEor
doEor:
    eor #$1d
noEor:
    sta seed
    rts

.segment "DATA"

sprite0: .tag sprite
.byte $d0      ; end of sprites marker


.rodata
gaps_start_pos:
    .byte $68   ; right down
    .byte $68   ; right down
    .byte $68   ; right down
    .byte $68   ; right down

    .byte $68   ; left up
    .byte $68   ; left up
    .byte $68   ; left up
    .byte $68   ; left up
jack_start_state: .byte 1
jack_start_anim: .byte 1
jack_start_current_frame: .byte 0
jack_start_jump_frame: .byte $ff

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

WINNER: .byte "You WIN!", 10, 13, 0
font_start:
.include "font.s"
font_end:

sprite_start:
.include "sprites.s"
sprite_end:
