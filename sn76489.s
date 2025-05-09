; vim: ft=asm_ca65 ts=4 sw=4 :
; Library functions for basic control of the SN76489 attached to the VIA

.include "io.inc"
.export sn_start, sn_stop, sn_silence, sn_note, sn_play_note, sn_send, sn_noise

FIRST   = %10000000
SECOND  = %00000000
CHAN_1  = %00000000
CHAN_2  = %00100000
CHAN_3  = %01000000
CHAN_N  = %01100000
TONE    = %00000000
VOL     = %00010000
VOL_OFF = %00001111
VOL_MAX = %00000000

SD_SCK  = %00000001
SD_CS   = %00000010
SN_WE   = %00000100
SN_READY= %00001000
SD_MOSI = %10000000

.zeropage

.code

sn_start:
    lda #(SD_SCK | SD_CS | SD_MOSI | SN_WE)
    sta via_ddra
    lda #$ff
    sta via_ddrb

    ; enable T1 Interupts
    lda #%01000000
    sta via_acr
    lda #$4e ; every 50000 (25ms) on a 2mhz clock
    sta via_t1cl
    lda #$c3
    sta via_t1ch
    jsr sn_silence
    rts

sn_stop:
    jsr sn_silence
    rts

sn_silence:
    lda #(FIRST|CHAN_1|VOL|VOL_OFF)
    jsr sn_send
    lda #(FIRST|CHAN_2|VOL|VOL_OFF)
    jsr sn_send
    lda #(FIRST|CHAN_3|VOL|VOL_OFF)
    jsr sn_send
    lda #(FIRST|CHAN_N|VOL|VOL_OFF)
    jsr sn_send
    rts

sn_noise:
    lda #4
    ora #(FIRST|CHAN_N)
    jsr sn_send
    lda #(FIRST|CHAN_N|VOL|VOL_MAX)
    jsr sn_send
    rts

sn_note:
    jsr sn_play_note
    ldy #$30
@d1:
    ldx #$00
@d2:
    dex
    bne @d2
    dey
    bne @d1
    jmp sn_silence

sn_play_note:

    lda #(FIRST|CHAN_1|TONE)
    jsr sn_send
    tya
    ora #(SECOND|CHAN_1|TONE)
    jsr sn_send
    lda #(FIRST|CHAN_1|VOL|$04)
    jsr sn_send
    rts

; Byte to send in A
sn_send:
    sta via_portb
    ldx #(SD_SCK|SD_CS|SD_MOSI|SN_WE)
    stx via_porta
    ldx #(SD_SCK|SD_CS|SD_MOSI)
    stx via_porta
    jsr sn_wait
    ldx #(SD_SCK|SD_CS|SD_MOSI|SN_WE)
    stx via_porta
    rts

sn_wait:
    lda via_porta
    and #SN_READY
    bne sn_wait
    rts

