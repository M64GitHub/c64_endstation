; 10 SYS (4096):REM a4 m64 endstation

*=$801

        BYTE    $22, $08, $0A, $00, $9E, $20, $28,  $34, $30, $39, $36, $29, $3a, $8f, $20, $41, $34, $20, $4D, $36, $34, $20, $45, $4E, $44, $53, $54, $41, $54, $49, $4F, $4E, $00, $00, $00

;===============================================================================
;
; M64 - MARiO64 SID-Wizard Sound Player V.00.01
;
;===============================================================================

; ------------------------------------------------------------------------------
; 10 SYS (4096)

;*=$801
;        BYTE    $0E, $08, $0A, $00, $9E, $20, $28,  $34
;        BYTE    $30, $39, $36, $29, $00, $00, $00



INIT_SOUND      = $4000
PLAY_SOUND      = $4003 


RASTERBAR_POS   = $12
SPRY            = 170

TITLEPOS        = $0400+410

CHAN1POS = $0400+538;498
CHAN2POS = $0400+540;+500
CHAN3POS = $0400+542;+502

CHAN1COL = $d800+538;+498
CHAN2COL = $d800+540;+500
CHAN3COL = $d800+542;+502

CHANCOLON       = 1
CHANCOLOFF      = 14

MINUS           = 94
PLUS            =24

SWVARS = INIT_SOUND+$21


;-------------------------------------------------------------------------------


; --- MAIN ---

* = $1000

INIT            
       

               ; clr screen data
@loop1  

        ldy #255
        dey
        bne *-1

        lda #$20 

        sta $0400,x   
        sta $0500,x   
        sta $0600,x   
        sta $0700,x

        lda #$1         ; clr screen color
        sta $d800,x   
        sta $d900,x   
        sta $da00,x   
        sta $db00,x
        dex            
        bne @loop1

    ;;__
        
        
        jsr rloop
        lda #14
        sta $d020
        sta $d021
        
        jsr rloop
        lda #3
        sta $d020
        sta $d021

        jsr rloop
        lda #14
        sta $d020
        sta $d021

        jsr rloop
        lda #6
        sta $d020
        sta $d021

        jsr rloop
        lda #11
        sta $d020
        sta $d021

        jsr rloop
        lda #0
        sta $d020
        sta $d021

        jmp scrn_is_black
    ;;--

rloop   
        ldy #5
@ll1
        ldx #$ff
        dex
        bne *-1

        lda #0
        cmp $d012
        bne *-3

        ldx #$ff
        dex
        bne *-1

        dey
        bne @ll1
        rts


scrn_is_black




        lda #$00        ; screen black      
        ;sta $d020     
        ;sta $d021     
        tax           
        
        ; SETUP CHARSET
        lda $d018       ; bits 1..3 * $400 = addr
        and #%11110000
        ora #%00001000
        sta $d018 
        lda $d016       ; turn off multicolor for characters
        and #$ef        ; by cleaing Bit#4 of $D016
        sta $d016 


        jmp     do_fadein

continue





        ; speedup drawlogo
        lda #$04
        sta drl_sta_0400+2
        lda Logo_X
        sta drl_sta_0400+1

        lda #$d8
        sta drl_sta_d800+2
        lda Logo_X
        sta drl_sta_d800+1


        jsr draw_logo

          ; speedup drawlogo
        lda #$04
        sta drl_sta_0400+2
        lda Logo_X
        sta drl_sta_0400+1

        lda #$d8
        sta drl_sta_d800+2
        lda Logo_X
        sta drl_sta_d800+1
        
        jsr waitkey

        ldy #1
        jsr printstartmsg

        lda #0
        cmp $d012
        bne *-3

        lda #255
        cmp $d012
        bne *-3
        
        ldx #$ff
        dex
        bne *-1

        lda #0
        cmp $d012
        bne *-3

        lda #1
        sta $d020
        sta $d021

        lda #255
        cmp $d012
        bne *-3
        

        ldx #$ff
        dex
        bne *-1

        lda #0
        cmp $d012
        bne *-3

        lda #0
        sta $d020
        sta $d021

        ldx #13
@loop_t
        lda title_fade1,y
        sta $d800+405,x

        lda title_fade2,y
        sta $d800+416,x
        
        dex
        bne @loop_t

        ldy #00         ; 
        jsr printsongmsg

        ; more txt

        lda #$2f  ; '/'
        sta $404+520
        lda #'2'
        sta $405+520
        lda #'a'
        sta $406+520

        lda #5
        sta $d802+520
        sta $d803+520
        sta $d805+520
        sta $d806+520
        lda #13
        sta $d804+520

        ;

        lda #2
        sta $d81e+520
        sta $d81f+520

        sta $d821+520
        sta $d822+520

        sta $d824+520
        sta $d825+520

        lda #10
        sta $d820+520
        sta $d823+520
        lda #$2f  ; '/'
        sta $420+520
        sta $423+520

        lda #0          ; tune #0
        jsr INIT_SOUND 

        ; SETUP SPRITES @2400
        ldx #$90
        stx $07f8
        inx
        stx $07f9
        inx
        stx $07fa
        inx
        stx $07fb
        inx
        stx $07fc
        inx
        stx $07fd
        inx
        stx $07fe
        inx
        stx $07ff

        lda #%00000000
        sta $d015 ; 1-8 sprites ON
        lda #$ff
        sta $d01c ; 1-8 sprites multicolor OFF
        
        lda #6    ; 14LBlu, 4 lila
        sta $d027 ; sprcolor spr #0
        sta $D028 ; sprcolor spr #1
        sta $D029 ; sprcolor spr #2
        sta $D02a ; sprcolor spr #3        
        sta $D02b ; sprcolor spr #4
        sta $D02c ; sprcolor spr #5
        sta $D02d ; sprcolor spr #6
        sta $D02e ; sprcolor spr #7
        
        ; X-COORDS
        lda #0
        sta $d010 ; hi bit x for all = 0

        ; X
        lda #0
        sta $d000
        sta $d002
        sta $d004
        sta $d006
        sta $d008
        sta $d00a
        sta $d00c
        sta $d00e
        
        ; Y
        lda #SPRY
        sta $d001
        sta $d003
        sta $d005
        sta $d007
        sta $d009
        sta $d00b
        sta $d00d
        sta $d00f

; FILL DOTS
        lda #00
        ldx #80
@dl1
        sta $0400+599,x
        sta $0400+639,x
        sta $0400+719,x
        sta $0400+799,x
       
        dex
        bne @dl1

        ; CHAR ANI
        lda #$4b
        sta CHAN1POS-2
        lda #11
        sta CHAN1COL-2

        lda ac_1
        sta CHAN1POS-1
        sta CHAN1POS+1

        lda ac_2
        sta CHAN3POS-1
        sta CHAN3POS+1

        lda #4
        sta CHAN1COL-1
        sta CHAN1COL+1
        sta CHAN3COL-1
        sta CHAN3COL+1
 
        lda #$4c
        sta CHAN3POS+2
        lda #11
        sta CHAN3COL+2

        ldx #240

@dotloop        lda dotcolors,x
                sta $da57,x
                dex
                bne @dotloop

        lda #0
        sta $d01b
        
IRQINIT
        sei 

        ldy #$7f  ; $7f = %01111111
        sty $dc0d ; Turn off CIAs Timer interrupts
        sty $dd0d ; Turn off CIAs Timer interrupts
        lda $dc0d ; cancel all CIA-IRQs in queue/unprocessed
        lda $dd0d ; cancel all CIA-IRQs in queue/unprocessed
        lda #$01  ; Set Interrupt Request Mask...
        sta $d01a ; ...we want IRQ by Rasterbeam
                
        lda #0  ; interrupt at line 0 
        sta $d012
         
        lda $d011 ; Bit#0 of $d011 is basically...
        and #$7f ; ...the 9th Bit for $d012    

        sta $d011 ; we need to make sure it is set to zero

        lda #1
        sta $d019 ; raster compare iq flag = 1 
        

        lda $01                           ;Inhalt von $01 in den Akku
        and #%11111101                    ;KERNAL- & BASIC-ROM
        sta $01                           ;ausblenden
        lda #<IRQ ; point IRQ Vector to our custom irq routine
        ldx #>IRQ
        sta $fffe  
        stx $ffff
                
        ;jsr irq0_init

        cli ; clear interrupt disable flag

        ;lda $d011
        ;and #%11101111
        ;sta $d011

        jmp * ; infinite loop

        rts


FRAMECTR BYTE $ff

align 256
; -----------------
; * IRQ / MAIN LOOP
IRQ     dec $d019 ; acknowledge IRQ

        inc FRAMECTR

        ror $2003
        bcc @lala
        lda #%10000000
        sta $2003                                
@lala

                

        jsr char_ani

        jsr logo_scroll_lr  
        
;        lda #117;#137
;        clc
;        sbc $d016
;        
;        sta $d000
;        lda #97
;        sta $d001
        lda #%11111110
        sta $d01c
        ;;;lda #13    

        lda SWVARS+34
        cmp #$a
        bcs @dodo

        lda #13   ; faarbe
        jmp @sk2

@dodo
   
        inc a4_color_ctr
        lda a4_color_ctr
        ;cmp #128
        bne @sk
        lda #0
        sta a4_color_ctr
        
@sk
        clc
        and #%11111100
        ror
        ror
        tax

        lda a4_colors,x
@sk2       
        sta $D027

@skk
        lda #$98
        sta $07f8
        lda #118 ;#117;#11;#81
        clc
        sbc $d016
        
        sta $d000
        lda #97;#81
        sta $d001

        jsr rasta1

        jsr draw_logo
        ;inc $d021
 
        lda #0
        sta $d016
        
        jsr sound_vis

        jsr move_sprites          
        

        jsr PLAY_SOUND

        jsr logo_color_ani
        





        lda #226
        cmp $d012
        bne *-3

;--
        lda SWVARS+34
        cmp #4
        bcc @skip1
;-
        jsr scroller
        jsr rasta2

        lda #0
        cmp DO1
        beq @nospeedscroll
  
        jsr scroller
 
@nospeedscroll
       jmp @skip2


@skip1     
        jsr rasta2

@skip2   
        
        lda lsrl_buf
        sta $d016

        lda SWVARS+34
        cmp #$8 ; 8!!!!!!!!!!!!!!1
        bcc @skip3
        jsr move_sprites2       


@skip3

        lda #0
        cmp @stuffflag
        beq @dosc
        
        sta @stuffflag
        
                        jsr fade_msgs 


;--

        lda SWVARS+34
        cmp #$6
        bcc @skip4
                        inc fade_ctr_t
                        ldy fade_ctr_t
                        cpy #64

                        bne @goon_t
                        ldy #0
                        sty fade_ctr_t

@goon_t

                        ldx #13
@loop_t
                        lda title_fade1,y
                        sta $d800+405,x
                       

                        lda title_fade2,y
                        sta $d800+416,x
                        
                        dex
                        bne @loop_t
; --
@skip4
       

        jmp @cont01

@dosc                        
                        jsr move_scrollcolors         
                        jsr do_vis

        lda #1
        sta @stuffflag

@cont01        
        
        ; speedup drawlogo
        lda #$04
        sta drl_sta_0400+2
        lda Logo_X
        sta drl_sta_0400+1

        lda #$d8
        sta drl_sta_d800+2
        lda Logo_X
        sta drl_sta_d800+1
 
        rti       

@stuffflag byte 0

a4_color BYTE 13
a4_color_ctr BYTE 0


do_vis

        ; 43 = INST NR = CURINST
        
        
; 34 = curr seq pos

        lda SWVARS+34 
        and #%11110000
        clc
        ror
        ror
        ror
        ror
        tax
        lda bin2hextbl,x
        sta $402+520
        lda SWVARS+34
        and #%00001111
        tax
        lda bin2hextbl,x
        sta $403+520
        

;        lda SWVARS+41
;        and #%11110000
;        ror
;        ror
;        ror
;        ror
;        tax
;        lda bin2hextbl,x
;        sta $402+720
;        lda SWVARS+41
;        and #%00001111
;        tax
;        lda bin2hextbl,x
;        sta $403+720


;        lda SWVARS+48
;        and #%11110000
;        ror
;        ror
;        ror
;        ror
;        tax
;        lda bin2hextbl,x
;        sta $402+800
;        lda SWVARS+48
;        and #%00001111
;        tax
;        lda bin2hextbl,x
;        sta $403+800




; 37 = curr pattern

        lda SWVARS+43 
        and #%11110000
        ror
        ror
        ror
        ror
        tax
        lda bin2hextbl,x
        sta $41e+520
        lda SWVARS+43
        and #%00001111
        tax
        lda bin2hextbl,x
        sta $41f+520
        

        lda SWVARS+50
        and #%11110000
        ror
        ror
        ror
        ror
        tax
        lda bin2hextbl,x
        sta $421+520
        lda SWVARS+50
        and #%00001111
        tax
        lda bin2hextbl,x
        sta $422+520


        lda SWVARS+57
        and #%11110000
        ror
        ror
        ror
        ror
        tax
        lda bin2hextbl,x
        sta $424+520
        lda SWVARS+57
        and #%00001111
        tax
        lda bin2hextbl,x
        sta $425+520



        rts





        

; --- SOUNDVIS ---
;varpos=PLAYERADDR+$21 ;variables start here - must be fixed address from now on...
;variables for channel1, (for other channels 7 / 14 must be added to the address):
;SPDCNT=varpos+1*3*7+1 ;speed/tempo-counter (to grab finer-grade moments)
;SEQPOS=varpos+1*3*7+2 ;sequence-position on channel 1 - useful for demo-timing
;CURPTN=varpos+2*3*7+0 ;currently played pattern (for demo maybe)
;CURNOT=varpos+2*3*7+1 ;current note
;CURIFX=varpos+2*3*7+3 ;current instrument/FX column value
;CURINS=varpos+2*3*7+4 ;currently selected instrument, must be changed temporarily
;CURFX2=varpos+2*3*7+5 ;small pattern-effect can be used to control volume of SFX



sound_vis

        lda #4
        sta CHAN1COL-1
        sta CHAN1COL+1
        sta CHAN3COL-1
        sta CHAN3COL+1

        lda SWVARS+1
        cmp last1
        bne @go1
        lda ac_3
        sta CHAN1POS
        ;lda $d015
        ;and #%11110110
        ;sta $d015 
        lda #CHANCOLOFF
        sta CHAN1COL
        sta $D025 
         
        lda #0
        sta DO1

        jmp @go11
        
@go1    sta last1
        lda #$47
        sta CHAN1POS
        lda #CHANCOLON
        sta CHAN1COL
        lda #1
        sta DO1


        lda #7
        sta CHAN1COL-1
        sta CHAN3COL+1

        lda #1
        sta CHAN1COL+1
        sta CHAN3COL-1

        sta $D025 


        ;lda $d015
        ;ora #%00001001
        ;sta $d015 

        ror $2003
        bcc @lala
        lda #%10000000
        sta $2003                                
@lala

        lda SWVARS+34
        cmp #4
        bcc @go11

        jsr logo_scroll_lr        
        jsr logo_scroll_lr   
        jsr logo_scroll_lr   
        jsr logo_scroll_lr  
        jsr move_sprites
        jsr move_sprites
        jsr move_sprites
 


        ;jsr scroller

@go11

        lda SWVARS+8
        cmp last2
        bne @go2
        lda ac_3
        sta CHAN2POS
        ;lda $d015
        ;and #%11101101
        ;sta $d015 
        lda #CHANCOLOFF
        sta CHAN2COL
        lda #0
        sta DO2

        jmp @go22
        
@go2    sta last2
        lda #$47
        sta CHAN2POS
        ;lda $d015
        ;ora #%00010010
        ;sta $d015
        lda #CHANCOLON
        sta CHAN2COL
        lda #1
        sta DO2

@go22

        lda SWVARS+15
        cmp last3
        bne @go3
        lda ac_3
        sta CHAN3POS
        ;lda $d015
        ;and #%11011011
        ;sta $d015 
        lda #CHANCOLOFF
        sta CHAN3COL
        lda #0
        sta DO3

        jmp @go33
        
@go3    sta last3
        lda #$47
        sta CHAN3POS
        ;lda $d015
        ;ora #%00100100
        ;sta $d015
        lda #CHANCOLON
        sta CHAN3COL
        lda #1
        sta DO3

@go33

        rts




;------------------
; * move sptites

sprmov_offs BYTE 0
sprmov_yoffs BYTE 0

move_sprites
                lda #$ff
                sta $d01c
                ;and #%00000111
                
; COLORS
                ;lda #11    ; DBlu
                ;sta $d025 ; multicolor 1

                lda #0
                sta $d01b

                lda #1   
                sta $d026 ; multicolor 2
        
                lda #6    ; 14LBlu, 4 lila
                sta $d027 ; sprcolor spr #0
                lda #7
                sta $D028 ; sprcolor spr #1
                lda #8
                sta $D029

; PTRs
                clc
                ldx #$0
                lda #$90

@loop2
                sta $7f8,x
                adc #1
                inx 
                cpx #$8
                bne @loop2
                
                
                lda #$d4
                sta $d001

                lda #$cf
                sta $d003
                
                lda #$c3
                sta $d005

                
                lda #$b8
                sta $d007

                lda #$af
                sta $d009

                lda #$a8
                sta $d00b

                lda #$a4
                sta $d00d

                lda #$a4
                sta $d00f



; X                               
                               
                inc sprmov_offs
                lda sprmov_offs
                cmp #85 ; -----------------------
                bne @l2
                lda #0
                sta sprmov_offs


                





@l2             tax

                lda sin_x5,x
                sta $d000
               
                txa
                adc #8
                tax
                lda sin_x5,x
                sta $d002
                
                txa
                adc #8
                tax
                lda sin_x5,x
                sta $d004

                txa
                adc #8
                tax
                lda sin_x5,x
                sta $d006

                txa
                adc #8
                tax
                lda sin_x5,x
                sta $d008

                txa
                adc #8
                tax
                lda sin_x5,x
                sta $d00a

                txa
                adc #8
                tax
                lda sin_x5,x
                sta $d00c

                txa
                adc #8
                tax
                lda sin_x5,x
                sta $d00e

                lda #%11111111
                sta $d015
               
                rts



sprmov_offs2 BYTE 0
sprmov_yoffs2 BYTE 0

move_sprites2  
; COLORS

                lda #%11111110
                sta $d01c


                lda #1
                sta $D027 ; sprcolor spr #0
;       
                lda #6
                sta $D028 ; sprcolor spr #1
                lda #6

                sta $d029 ; sprcolor spr #2
                lda #$6
                sta $D02a ; sprcolor spr #3


              


; PTRs
                clc
                ldx #$0
                lda #$98

@loop2
                sta $7f8,x
                adc #1
                inx 
                cpx #$8
                bne @loop2


; X
                inc sprmov_offs2
                lda sprmov_offs2
                cmp #100
                beq @testpriority

;                cmp #50
;                beq @testp2
;                cmp #99
;                beq @testp2

                jmp @l2

;@testp2
;                pha

;                lda #0
;                cmp SPRPRIORITY
;                beq @itszero2
;                lda #$00
;                sta SPRPRIORITY
;                jmp @tol22


;@itszero2        lda #$ff
;                sta SPRPRIORITY
;                lda #0

;                
;@tol22          pla
;                jmp @l2




@testpriority
                
                lda #0
                sta sprmov_offs2


;                lda #0
;                cmp SPRPRIORITY
;                beq @itszero
;                lda #$00
;                sta SPRPRIORITY
;                jmp @tol2


;@itszero        lda #$ff
;                sta SPRPRIORITY
;                lda #0



                
@tol2           


@l2             ;tax

                ;lda sin_x2,x
                ;sta $d000
                
               
                ;txa
                adc #6
                tax
                lda sin_x2,x
                sta $d002
                
                txa
                adc #6
                tax
                lda sin_x2,x
                sta $d004

                txa
                adc #6
                tax
                lda sin_x2,x
                sta $d006

                txa
                adc #6
                tax
                lda sin_x2,x
                sta $d008

                txa
                adc #6
                tax
                lda sin_x2,x
                sta $d00a

                txa
                adc #6
                tax
                lda sin_x2,x
                sta $d00c

                txa
                adc #6
                tax
                lda sin_x2,x
                sta $d00e

        ; move Y
                
                inc sprmov_yoffs2
                lda sprmov_yoffs2
                cmp #170
                bne @l3
                lda #0
                sta sprmov_yoffs2
                ;jmp @l3

@check          
;                cmp #40
;                beq @doitt
;                cmp #84
;                beq @doitt

;                jmp @l3
;@doitt
;                pha

;                lda #0
;                cmp SPRPRIORITY
;                beq @itszero
;                lda #$00
;                sta SPRPRIORITY
;                jmp @tol3


;@itszero        lda #$ff
;                sta SPRPRIORITY
;                lda #0

;@tol3           pla





@l3             tax

                lda sin_x5,x
                sta $d001
                ;sta $d003
                ;sta $d005
                ;sta $d007
                ;sta $d009

                ;rts


                txa
                adc #8
                tax
                lda sin_x5,x
                clc
                sbc #MINUS
                sta $d003
                
                txa
                adc #4
                tax
                lda sin_x5,x
                clc
                sbc #MINUS
                sta $d005

                txa
                adc #4
                tax
                lda sin_x5,x
                clc
                sbc #MINUS
                sta $d007

                txa
                adc #4
                tax
                lda sin_x5,x
                clc
                sbc #MINUS
                sta $d009

                txa
                adc #4
                tax
                lda sin_x5,x
                clc
                sbc #MINUS
                sta $d00b

                txa
                adc #4
                tax
                lda sin_x5,x
                clc
                sbc #MINUS
                sta $d00d

                txa
                adc #4
                tax
                lda sin_x5,x
                clc
                sbc #MINUS
                sta $d00f

                rts


SPRPRIORITY byte $00


; --- RASTA 1 ---
rasta1          
                ldy #RASTERBAR_POS
                ldx #$00
                
@l1             lda rb_colors1,x 

                cpy $d012 
                bne *-3   

                sta $d020 
                sta $d021
                
                cpx #24
                beq @xit 

                inx      
                iny      

                jmp @l1

@xit
                rts

                ;inc @l1+1
                lda @l1+1
                clc
                adc #25
                bcc @xit2

                inc @l1+2
                ldx @l1+2
                cpx #>rb_colors1+2
                beq @xit3
                stx @l1+2

@xit2
                sta @l1+1
                rts

@xit3           lda #<rb_colors1
                sta @l1+1
                lda #>rb_colors1
                sta @l1+2
                rts






; --- RASTA2 ---
rasta2          ldy #2
                ldx #$00
                
@l1             lda rb_colors2,x 

                cpy $d012 
                bne *-3   

                sta $d020 
                sta $d021
                
                cpx #28
                beq @xit 

                inx      
                iny      

                jmp @l1

@xit
          rts
                ;inc @l1+1
                lda @l1+1
                clc
                adc #29
                bcc @xit2

                inc @l1+2
                ldx @l1+2
                cpx #>rb_colors2+2
                beq @xit3
                stx @l1+2

@xit2
                sta @l1+1
                rts

@xit3           lda #<rb_colors2
                sta @l1+1
                lda #>rb_colors2
                sta @l1+2
                rts




ac_1    BYTE 90
ac_2    BYTE 97
ac_3    BYTE $68

char_ani_delay byte 2


char_ani
        dec char_ani_delay
        beq @do_it
        rts

        
@do_it                
        lda #4 ;5
        sta char_ani_delay

        inc ac_1
        dec ac_2
                
        lda ac_1
        cmp #97
        bne @goonn

        lda #90
        sta ac_1
        lda #97
        sta ac_2

@goonn

        inc ac_3
        lda ac_3
        cmp #$6c
        bne @goon2

        lda #$68
        sta ac_3

@goon2

; CHAR ANI

        lda ac_1
        sta CHAN1POS-1
        sta CHAN1POS+1

        lda ac_2
        sta CHAN3POS-1
        sta CHAN3POS+1
 
        rts
                
   

; --- scroller ---

scroller 
        dec @scrollreg
        lda @scrollreg
        cmp #$ff
        beq @nextchar        
        sta $d016
        jmp @do_print

@nextchar
        lda #7
        sta @scrollreg
        sta $d016        
        inc @scrolltext_pos
@do_print
        ldx @scrolltext_pos
        ldy #0
@printchar
        lda @scrolltext,x
        cmp #$ff
        beq @endscroll
        ;sta $4c8,y ;$04a0,y
        sta $07c0,y
        ;sta $4f0,y
        inx
        iny
        cpy #40
        bne @printchar
        rts
@endscroll
        lda #0
        sta @scrolltext_pos
        rts
        
@scrollreg BYTE 0
@scrolltext_pos BYTE 0
@scrolltext     text '                                        '
                ;byte $47
                text 'mario64 presents  ' 
                byte 80,0
                text 'endstation'
                byte 0,80

                text '  the endpart music for the a4 bcc#10 demo '
                
                byte 80,0
                text 'from'
                byte 0
                text 'berlin'
                byte 0
                text 'to'
                byte 0
                text 'paris'
                byte 0
                text 'and'
                byte 0
                text 'back'
                byte 0,80

               ;text '  so i have coded a new player for my music. and tried to make some fancy sync of the gfx and music playing '
                text '  so i have coded a new player for my music. and tried to make some fancy sync of the gfx and music playing '

                byte 80,0
                text 'enjoy the ride ...'
                byte 0,80

                text '                                        '
 BYTE $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff



move_scrollcolors 

                inc @scrollcolptr
                ldx @scrollcolptr
                cpx #64
                bne @goon1
                ldx #0
                stx @scrollcolptr
@goon1          
                ldy #0
@dotloop2       lda scrollcolors,x
                sta $dbc0,y
                iny 
                inx
                cpy #40
                bne @dotloop2

                rts
                               
@scrollcolptr byte 0



; --- ROUTINES -----------------------------------------------------------------

logo_scroll_lr
        lda SWVARS+34
        cmp #$a
        bcs @skip1
        rts

@skip1


        lda lsr_ctr
        tax
        lda lsrl_table1,x
        cmp #$ff
        beq @end
        ;sta $d016

        sta lsrl_buf

        ;lda lsrl_table2,x
        ;sta LOGO_X
        
        inx
        txa
        sta lsr_ctr
        rts

@end    lda #0
        sta lsr_ctr
        rts
     


logo_color_ani
;        lda FRAMECTR      
;        cmp #8
;        bcc @do_it
;        rts

        lda #1
        cmp DO1
        beq @cont1set
        lda #0
                
@cont1set
        pha

        lda #1
        cmp DO2
        bne @cont2clear
        pla
        ora #%00000010
        pha

@cont2clear
        lda #1
        cmp DO3
        bne @cont3clear
        pla
        ora #%00000100
        pha

@cont3clear
        pla
;        cmp #0
;        beq @end

;        sta @last_scrn        

        tax

;        jmp @gooon


;        



;@end    ;lda @last_scrn
;        ;cmp #0
;        ;beq @clear

;        inc @stay_ctr
;        lda @stay_ctr
;        cmp #4
;        beq @doclear
;        
;        ldx @last_scrn
;        jmp @gooon
;        

;@doclear lda #0
;        sta @stay_ctr
;        sta @last_scrn

;@clear
;        ldx #0

;@gooon

        txa        
        asl
        tax
        lda DRL_ANI_TBL,x
        sta drl_lda_logo_color+1
        inx
        lda DRL_ANI_TBL,x
        sta drl_lda_logo_color+2
        rts

@stay_ctr byte 0
@last_scrn byte 0
@disp_mask byte 0


;        ldx DRL_ani_ctr
;        inx
;        cpx #7
;        beq @end
;        stx DRL_ani_ctr
;        txa
;        asl
;        tax
;        lda DRL_ANI_TBL,x
;        sta drl_lda_logo_color+1
;        inx
;        lda DRL_ANI_TBL,x
;        sta drl_lda_logo_color+2

;        rts

;@end     
;        lda #$ff
;        sta DRL_ani_ctr
;        rts






fade_msgs
        inc @delay
        lda @delay
        cmp #3
        beq @goon
        rts
@goon
        lda #0
        sta @delay

        lda current_msg
        asl
        tax
        lda message_table,x
        sta @printloop+1
        inx
        lda message_table,x
        sta @printloop+2

        ldx #0

@printloop
        lda message1,x
        cmp #$ff
        beq @endprint
        cmp #32
        bne @dosta
        lda #0
        
@dosta

        sta $700+5,x
        ;sta $700-35,x
        inx
        jmp @printloop
        
@endprint
        inc @fade_ctr
        lda @fade_ctr
        cmp #7
        bne @next_fade_step

        lda #0
        sta @fade_ctr

        cmp @fade_in_out
        bne @in_to_out ; when not 0 its current fadein -> do fadeout now

        ; faded out, increase msg_count
        lda #1
        sta @fade_in_out

        inc current_msg
        lda current_msg
        
        cmp #$f

        bne @in_to_out
        lda #0
        sta current_msg
        ;jmp @next_fade_step

;in_to_out
@in_to_out
        sta @fade_in_out
        lda #0
        jmp @next_fade_step


; next_fade_step
@next_fade_step
        tax
        lda @fade_in_out
        cmp #00
        bne @fade_out
@fade_in
        lda fade_in_colors,x
        jmp @goon1
@fade_out
        lda fade_out_colors,x

@goon1  
        ldx #16
@loop1
        sta $db00+4,x
        dex
        bne @loop1

;
@endme


        rts

@fade_ctr byte 0

@fade_in_out byte 1 ; 1 in, 0 out

@fade_stay_ctr byte 0
        
@delay byte 0



draw_logo
;        lda #$04
;        sta drl_sta_0400+2
;        lda Logo_X
;        sta drl_sta_0400+1

;        lda #$d8
;        sta drl_sta_d800+2
;        lda Logo_X
;        sta drl_sta_d800+1


        ldx #0
        
drl_loop_outer
        ldy #24

drl_loop_inner
                lda M64LOGO_chars,x
drl_sta_0400    sta $0400,x

drl_lda_logo_color
                lda M64LOGO_color,x

                ;cmp #03
                ;bne drl_sta_d800
                ;lda $dbc0

drl_sta_d800    sta $d800,x

                inx                

                dey
                bne drl_loop_inner

                cpx #192
                beq drl_end

        lda drl_sta_0400+1
        ;clc
        adc #16
        bne @goon
        inc drl_sta_0400+2
@goon   sta drl_sta_0400+1

        lda drl_sta_d800+1
        ;clc
        adc #16
        bne @goon2
        inc drl_sta_d800+2
@goon2   sta drl_sta_d800+1


        jmp drl_loop_outer

drl_end 

        rts




        

; ---


waitkey
        lda #$0
        sta $dc03       ; port b ddr (input)
        lda #$ff
        sta $dc02       ; port a ddr (output)
                        
        lda #$00
        sta $dc00       ; port a
        lda $dc01       ; port b
        cmp #$ff
        beq waitkey
        
        clc
        rts



printstartmsg
        ldx #00
@loop
        lda startmsg,x
        cmp #$ff
        beq @end
        cpy #1
        bne @storechar
        lda #32
@storechar
        sta $668,x
        inx
        jmp @loop
@end
        rts
        

startmsg
 BYTE 'hit the long one ...'
 BYTE $ff


printsongmsg
        ldx #00
@loop
        lda SONGMSG,x
        cmp #$ff
        beq @end
        cpy #1
        bne @storechar
        lda #32
@storechar
        sta TITLEPOS,x
        inx
        jmp @loop
@end
        rts
        

SONGMSG
 text 'playing '
 byte 75
 text 'endstation'
 byte 76, $ff


;-------------------------------------------------------------------------------
; CHARSET DATA
*=$2000
CHARSET
incbin "chars.cst",0,107


* = $2400
SPRDATA_0xF
incbin "m64.spt",1,8,true


* = $2600
SPRDATA_SMALL
incbin "m64a4.spt",1,8,true


scrollcolors
 byte 11, 11, 12, 12, 4, 4, 12, 12, 14, 14, 3, 3, 1, 1  ; 14
 byte 1, 1, 3, 3, 14, 14, 12, 12, 4, 4, 12, 12, 11, 11  ; 14

 byte 11, 5, 13, 3, 1, 3, 13, 11                        ;  8
 byte 0, 11, 2, 10, 1, 10, 2, 11                        ;  8
 
 byte 11, 11, 4, 4, 10, 10, 7, 7, 1, 1                  ; 10
 byte 1, 1, 7, 7, 10, 10, 4, 4, 11, 11                  ; 10




 byte 11, 11, 12, 12, 4, 4, 12, 12, 14, 14, 3, 3, 1, 1  ; 14
 byte 1, 1, 3, 3, 14, 14, 12, 12, 4, 4, 12, 12, 11, 11  ; 14

 byte 11, 5, 13, 3, 1, 3, 13, 11                        ;  8
 byte 0, 11, 2, 10, 1, 10, 2, 11                        ;  8
 
 byte 11, 11, 4, 4, 10, 10, 7, 7, 1, 1                  ; 10
 byte 1, 1, 7, 7, 10, 10, 4, 4, 11, 11                  ; 10






 byte 1,1,1,1,1,1,1,1

dotcolors
 byte 1, 1, 1, 1, 15, 15, 15, 15, 13, 13, 13, 13, 5, 5, 5, 5
 byte 11, 11, 11, 11, 5, 5, 5, 5, 13, 13, 13, 13, 15, 15, 15, 15
 byte 1, 1, 1, 1, 15, 15, 15, 15, 13, 13, 13, 13, 5, 5, 5, 5
 byte 11, 11, 11, 11, 5, 5, 5, 5, 13, 13, 13, 13, 15, 15, 15, 15 
 byte 1, 1, 1, 1, 15, 15, 15, 15, 14, 14, 14, 14, 6, 6, 6, 6
 byte 11, 11, 11, 11, 6, 6, 6, 6, 14, 14, 14, 14, 15, 15, 15, 15
 byte 1, 1, 1, 1, 15, 15, 15, 15, 14, 14, 14, 14, 6, 6, 6, 6
 byte 11, 11, 11, 11, 6, 6, 6, 6, 14, 14, 14, 14, 15, 15, 15, 15

 byte 1, 1, 1, 1, 15, 15, 15, 15, 13, 13, 13, 13, 5, 5, 5, 5
 byte 11, 11, 11, 11, 5, 5, 5, 5, 13, 13, 13, 13, 15, 15, 15, 15
 byte 1, 1, 1, 1, 15, 15, 15, 15, 13, 13, 13, 13, 5, 5, 5, 5
 byte 11, 11, 11, 11, 5, 5, 5, 5, 13, 13, 13, 13, 15, 15, 15, 15 
 byte 1, 1, 1, 1, 15, 15, 15, 15, 14, 14, 14, 14, 6, 6, 6, 6
 byte 11, 11, 11, 11, 6, 6, 6, 6, 14, 14, 14, 14, 15, 15, 15, 15
 byte 1, 1, 1, 1, 15, 15, 15, 15, 14, 14, 14, 14, 6, 6, 6, 6
 byte 11, 11, 11, 11, 6, 6, 6, 6, 14, 14, 14, 14, 15, 15, 15, 15

 byte 1, 1, 1, 1, 15, 15, 15, 15, 13, 13, 13, 13, 5, 5, 5, 5
 byte 11, 11, 11, 11, 5, 5, 5, 5, 13, 13, 13, 13, 15, 15, 15, 15
 byte 1, 1, 1, 1, 15, 15, 15, 15, 13, 13, 13, 13, 5, 5, 5, 5
 byte 11, 11, 11, 11, 5, 5, 5, 5, 13, 13, 13, 13, 15, 15, 15, 15 
 byte 1, 1, 1, 1, 15, 15, 15, 15, 14, 14, 14, 14, 6, 6, 6, 6
 byte 11, 11, 11, 11, 6, 6, 6, 6, 14, 14, 14, 14, 15, 15, 15, 15
 byte 1, 1, 1, 1, 15, 15, 15, 15, 14, 14, 14, 14, 6, 6, 6, 6
 byte 11, 11, 11, 11, 6, 6, 6, 6, 14, 14, 14, 14, 15, 15, 15, 15

 byte 1, 1, 1, 1, 15, 15, 15, 15, 13, 13, 13, 13, 5, 5, 5, 5
 byte 11, 11, 11, 11, 5, 5, 5, 5, 13, 13, 13, 13, 15, 15, 15, 15
 byte 1, 1, 1, 1, 15, 15, 15, 15, 13, 13, 13, 13, 5, 5, 5, 5
 byte 11, 11, 11, 11, 5, 5, 5, 5, 13, 13, 13, 13, 15, 15, 15, 15 
 byte 1, 1, 1, 1, 15, 15, 15, 15, 14, 14, 14, 14, 6, 6, 6, 6
 byte 11, 11, 11, 11, 6, 6, 6, 6, 14, 14, 14, 14, 15, 15, 15, 15
 byte 1, 1, 1, 1, 15, 15, 15, 15, 14, 14, 14, 14, 6, 6, 6, 6
 byte 11, 11, 11, 11, 6, 6, 6, 6, 14, 14, 14, 14, 15, 15, 15, 15


; SOUND DATA
*=$4000
incbin "endp0062.sid", $7e
;-------------------------------------------------------------------------------
ENDSID BYTE 0

*=$5100
M64LOGO_color
M64LOGO_color0
; Screen 1 - Colour data
        BYTE    $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
        BYTE    $01,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$01,$0E,$0E,$0E,$0E,$0E,$0E,$01,$0E,$0E,$0E,$01,$0E,$0E,$0E,$01
        BYTE    $01,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$01,$0E,$0E,$0E,$01,$01,$01,$01,$0E,$0E,$0E,$01,$0E,$0E,$0E,$01
        BYTE    $0F,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$01,$0E,$0E,$0E,$0E,$0E,$0E,$01,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0F
        BYTE    $0C,$0E,$0E,$01,$0E,$01,$0E,$0E,$01,$0E,$0E,$0E,$01,$0E,$0E,$01,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0C
        BYTE    $0B,$0E,$0E,$01,$0E,$01,$0E,$0E,$01,$0E,$0E,$0E,$01,$0E,$0E,$01,$01,$01,$01,$01,$0E,$0E,$0E,$0B
        BYTE    $0B,$0E,$0E,$01,$0E,$01,$0E,$0E,$01,$0E,$0E,$0E,$0E,$0E,$0E,$0B,$03,$06,$03,$01,$0E,$0E,$0E,$0B
        BYTE    $0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$03,$03,$03,$01,$0B,$0B,$0B,$0B
M64LOGO_color1
; Screen 2 - Colour data
        BYTE    $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
        BYTE    $01,$03,$03,$03,$03,$03,$03,$03,$01,$0E,$0E,$0E,$0E,$0E,$0E,$01,$0E,$0E,$0E,$01,$0E,$0E,$0E,$01
        BYTE    $01,$03,$03,$03,$03,$03,$03,$03,$01,$0E,$0E,$0E,$01,$01,$01,$01,$0E,$0E,$0E,$01,$0E,$0E,$0E,$01
        BYTE    $0F,$03,$03,$03,$03,$03,$03,$03,$01,$0E,$0E,$0E,$0E,$0E,$0E,$01,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0F
        BYTE    $0C,$03,$03,$01,$03,$01,$03,$03,$01,$0E,$0E,$0E,$01,$0E,$0E,$01,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0C
        BYTE    $0B,$03,$03,$01,$03,$01,$03,$03,$01,$0E,$0E,$0E,$01,$0E,$0E,$01,$01,$01,$01,$01,$0E,$0E,$0E,$0B
        BYTE    $0B,$03,$03,$01,$03,$01,$03,$03,$01,$0E,$0E,$0E,$0E,$0E,$0E,$0B,$03,$06,$03,$01,$0E,$0E,$0E,$0B
        BYTE    $0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$03,$03,$03,$01,$0B,$0B,$0B,$0B
M64LOGO_color2
; Screen 3 - Colour data
        BYTE    $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
        BYTE    $01,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$01,$03,$03,$03,$03,$03,$03,$01,$0E,$0E,$0E,$01,$0E,$0E,$0E,$01
        BYTE    $01,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$01,$03,$03,$03,$01,$01,$01,$01,$0E,$0E,$0E,$01,$0E,$0E,$0E,$01
        BYTE    $0F,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$01,$03,$03,$03,$03,$03,$03,$01,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0F
        BYTE    $0C,$0E,$0E,$01,$0E,$01,$0E,$0E,$01,$03,$03,$03,$01,$03,$03,$01,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0C
        BYTE    $0B,$0E,$0E,$01,$0E,$01,$0E,$0E,$01,$03,$03,$03,$01,$03,$03,$01,$01,$01,$01,$01,$0E,$0E,$0E,$0B
        BYTE    $0B,$0E,$0E,$01,$0E,$01,$0E,$0E,$01,$03,$03,$03,$03,$03,$03,$0B,$03,$06,$03,$01,$0E,$0E,$0E,$0B
        BYTE    $0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$03,$03,$03,$01,$0B,$0B,$0B,$0B
M64LOGO_color3
; Screen 4 - Colour data
        BYTE    $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
        BYTE    $01,$03,$03,$03,$03,$03,$03,$03,$01,$03,$03,$03,$03,$03,$03,$01,$0E,$0E,$0E,$01,$0E,$0E,$0E,$01
        BYTE    $01,$03,$03,$03,$03,$03,$03,$03,$01,$03,$03,$03,$01,$01,$01,$01,$0E,$0E,$0E,$01,$0E,$0E,$0E,$01
        BYTE    $0F,$03,$03,$03,$03,$03,$03,$03,$01,$03,$03,$03,$03,$03,$03,$01,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0F
        BYTE    $0C,$03,$03,$01,$03,$01,$03,$03,$01,$03,$03,$03,$01,$03,$03,$01,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0C
        BYTE    $0B,$03,$03,$01,$03,$01,$03,$03,$01,$03,$03,$03,$01,$03,$03,$01,$01,$01,$01,$01,$0E,$0E,$0E,$0B
        BYTE    $0B,$03,$03,$01,$03,$01,$03,$03,$01,$03,$03,$03,$03,$03,$03,$0B,$03,$06,$03,$01,$0E,$0E,$0E,$0B
        BYTE    $0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$03,$03,$03,$01,$0B,$0B,$0B,$0B
M64LOGO_color4
; Screen 5 - drl_lda_logo_colorColour data
        BYTE    $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
        BYTE    $01,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$01,$0E,$0E,$0E,$0E,$0E,$0E,$01,$03,$03,$03,$01,$03,$03,$03,$01
        BYTE    $01,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$01,$0E,$0E,$0E,$01,$01,$01,$01,$03,$03,$03,$01,$03,$03,$03,$01
        BYTE    $0F,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$01,$0E,$0E,$0E,$0E,$0E,$0E,$01,$03,$03,$03,$03,$03,$03,$03,$0F
        BYTE    $0C,$0E,$0E,$01,$0E,$01,$0E,$0E,$01,$0E,$0E,$0E,$01,$0E,$0E,$01,$03,$03,$03,$03,$03,$03,$03,$0C
        BYTE    $0B,$0E,$0E,$01,$0E,$01,$0E,$0E,$01,$0E,$0E,$0E,$01,$0E,$0E,$01,$01,$01,$01,$01,$03,$03,$03,$0B
        BYTE    $0B,$0E,$0E,$01,$0E,$01,$0E,$0E,$01,$0E,$0E,$0E,$0E,$0E,$0E,$0B,$03,$06,$03,$01,$03,$03,$03,$0B
        BYTE    $0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$03,$03,$03,$01,$0B,$0B,$0B,$0B
M64LOGO_color5
; Screen 6 - Colour data
        BYTE    $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
        BYTE    $01,$03,$03,$03,$03,$03,$03,$03,$01,$0E,$0E,$0E,$0E,$0E,$0E,$01,$03,$03,$03,$01,$03,$03,$03,$01
        BYTE    $01,$03,$03,$03,$03,$03,$03,$03,$01,$0E,$0E,$0E,$01,$01,$01,$01,$03,$03,$03,$01,$03,$03,$03,$01
        BYTE    $0F,$03,$03,$03,$03,$03,$03,$03,$01,$0E,$0E,$0E,$0E,$0E,$0E,$01,$03,$03,$03,$03,$03,$03,$03,$0F
        BYTE    $0C,$03,$03,$01,$03,$01,$03,$03,$01,$0E,$0E,$0E,$01,$0E,$0E,$01,$03,$03,$03,$03,$03,$03,$03,$0C
        BYTE    $0B,$03,$03,$01,$03,$01,$03,$03,$01,$0E,$0E,$0E,$01,$0E,$0E,$01,$01,$01,$01,$01,$03,$03,$03,$0B
        BYTE    $0B,$03,$03,$01,$03,$01,$03,$03,$01,$0E,$0E,$0E,$0E,$0E,$0E,$0B,$03,$06,$03,$01,$03,$03,$03,$0B
        BYTE    $0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$03,$03,$03,$01,$0B,$0B,$0B,$0B
M64LOGO_color6
; Screen 7 - Colour data
        BYTE    $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
        BYTE    $01,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$01,$03,$03,$03,$03,$03,$03,$01,$03,$03,$03,$01,$03,$03,$03,$01
        BYTE    $01,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$01,$03,$03,$03,$01,$01,$01,$01,$03,$03,$03,$01,$03,$03,$03,$01
        BYTE    $0F,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$01,$03,$03,$03,$03,$03,$03,$01,$03,$03,$03,$03,$03,$03,$03,$0F
        BYTE    $0C,$0E,$0E,$01,$0E,$01,$0E,$0E,$01,$03,$03,$03,$01,$03,$03,$01,$03,$03,$03,$03,$03,$03,$03,$0C
        BYTE    $0B,$0E,$0E,$01,$0E,$01,$0E,$0E,$01,$03,$03,$03,$01,$03,$03,$01,$01,$01,$01,$01,$03,$03,$03,$0B
        BYTE    $0B,$0E,$0E,$01,$0E,$01,$0E,$0E,$01,$03,$03,$03,$03,$03,$03,$0B,$03,$06,$03,$01,$03,$03,$03,$0B
        BYTE    $0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$03,$03,$03,$01,$0B,$0B,$0B,$0B
M64LOGO_color7
; Screen 8 - Colour data
        BYTE    $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
        BYTE    $01,$03,$03,$03,$03,$03,$03,$03,$01,$03,$03,$03,$03,$03,$03,$01,$03,$03,$03,$01,$03,$03,$03,$01
        BYTE    $01,$03,$03,$03,$03,$03,$03,$03,$01,$03,$03,$03,$01,$01,$01,$01,$03,$03,$03,$01,$03,$03,$03,$01
        BYTE    $0F,$03,$03,$03,$03,$03,$03,$03,$01,$03,$03,$03,$03,$03,$03,$01,$03,$03,$03,$03,$03,$03,$03,$0F
        BYTE    $0C,$03,$03,$01,$03,$01,$03,$03,$01,$03,$03,$03,$01,$03,$03,$01,$03,$03,$03,$03,$03,$03,$03,$0C
        BYTE    $0B,$03,$03,$01,$03,$01,$03,$03,$01,$03,$03,$03,$01,$03,$03,$01,$01,$01,$01,$01,$03,$03,$03,$0B
        BYTE    $0B,$03,$03,$01,$03,$01,$03,$03,$01,$03,$03,$03,$03,$03,$03,$0B,$03,$06,$03,$01,$03,$03,$03,$0B
        BYTE    $0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$03,$03,$03,$01,$0B,$0B,$0B,$0B
M64LOGO_chars
; Screen 1 - 
        BYTE    $46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46
        BYTE    $46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46
        BYTE    $46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46
        BYTE    $46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46
        BYTE    $46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46
        BYTE    $46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46
        BYTE    $46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$20,$20,$20,$46,$46,$46,$46,$46
        BYTE    $46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$20,$20,$20,$46,$46,$46,$46,$46

        
        
align 256
sin_x3 BYTE 53, 53, 53, 54, 54, 55, 55, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 55, 55, 54, 54, 53, 53, 53, 52, 52, 51, 51, 50, 50, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 50, 50, 51, 51, 52, 52, 52, 53, 53, 54, 54, 55, 55, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 55, 55, 54, 54, 53, 53, 53, 52, 52, 51, 51, 50, 50, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 50, 50, 51, 51, 52, 52

align 256
sin_x4 BYTE 157, 161, 165, 169, 173, 177, 180, 184, 187, 191, 194, 197, 200, 202, 205, 207, 209, 210, 212, 213, 214, 214, 214, 214, 214, 214, 213, 212, 210, 209, 207, 205, 202, 200, 197, 194, 191, 187, 184, 180, 177, 173, 169, 165, 161, 157, 153, 149, 145, 141, 137, 134, 130, 127, 123, 120, 117, 114, 112, 109, 107, 105, 104, 102, 101, 100, 100, 100, 100, 100, 100, 101, 102, 104, 105, 107, 109, 112, 114, 117, 120, 123, 127, 130, 134, 137, 141, 145, 149, 153, 157, 161, 165, 169, 173, 177, 180, 184, 187, 191, 194, 197, 200, 202, 205, 207, 209, 210, 212, 213, 214, 214, 214, 214, 214, 214, 213, 212, 210, 209, 207, 205, 202, 200, 197, 194, 191, 187, 184, 180, 177, 173, 169, 165, 161, 157, 153, 149, 145, 141, 137, 134, 130, 127, 123, 120, 117, 114, 112, 109, 107, 105, 104, 102, 101, 100, 100, 100, 100, 100, 100, 101, 102, 104, 105, 107, 109, 112, 114, 117, 120, 123, 127, 130, 134, 137, 141, 145, 149, 153

align 256
sin_x2 BYTE 157, 163, 169, 175, 181, 187, 193, 199, 204, 209, 214, 219, 224, 228, 232, 236, 239, 242, 245, 248, 250, 251, 253, 254, 254, 254, 254, 254, 253, 251, 250, 248, 245, 242, 239, 236, 232, 228, 224, 219, 214, 209, 204, 199, 193, 187, 181, 175, 169, 163, 157, 151, 145, 139, 133, 127, 121, 115, 110, 105, 100, 95, 90, 86, 82, 78, 75, 72, 69, 66, 64, 63, 61, 60, 60, 60, 60, 60, 61, 63, 64, 66, 69, 72, 75, 78, 82, 86, 90, 95, 100, 105, 110, 115, 121, 127, 133, 139, 145, 151, 157, 163, 169, 175, 181, 187, 193, 199, 204, 209, 214, 219, 224, 228, 232, 236, 239, 242, 245, 248, 250, 251, 253, 254, 254, 254, 254, 254, 253, 251, 250, 248, 245, 242, 239, 236, 232, 228, 224, 219, 214, 209, 204, 199, 193, 187, 181, 175, 169, 163, 157, 151, 145, 139, 133, 127, 121, 115, 110, 105, 100, 95, 90, 86, 82, 78, 75, 72, 69, 66, 64, 63, 61, 60, 60, 60, 60, 60, 61, 63, 64, 66, 69, 72, 75, 78, 82, 86, 90, 95, 100, 105, 110, 115, 121, 127, 133, 139, 145, 151
       BYTE 0, 0, 0, 0, 0, 0, 0, 0

align 256
sin_x5 BYTE 220, 219, 219, 219, 218, 217, 216, 214, 213, 211, 209, 207, 205, 202, 200, 197, 195, 192, 189, 186, 183, 180, 177, 174, 171, 169, 166, 163, 160, 158, 155, 153, 151, 149, 147, 145, 144, 143, 142, 141, 140, 140, 140, 140, 140, 140, 141, 142, 143, 144, 145, 147, 149, 151, 153, 155, 158, 160, 163, 166, 169, 171, 174, 177, 180, 183, 186, 189, 192, 195, 197, 200, 202, 205, 207, 209, 211, 213, 214, 216, 217, 218, 219, 219, 219, 219, 219, 219, 219, 218, 217, 216, 214, 213, 211, 209, 207, 205, 202, 200, 197, 195, 192, 189, 186, 183, 180, 177, 174, 171, 169, 166, 163, 160, 158, 155, 153, 151, 149, 147, 145, 144, 143, 142, 141, 140, 140, 140, 140, 140, 140, 141, 142, 143, 144, 145, 147, 149, 151, 153, 155, 158, 160, 163, 166, 169, 171, 174, 177, 180, 183, 186, 189, 192, 195, 197, 200, 202, 205, 207, 209, 211, 213, 214, 216, 217, 218, 219, 219, 219
       BYTE 220, 219, 219, 219, 218, 217, 216, 214, 213, 211, 209, 207, 205, 202, 200, 197, 195, 192, 189, 186, 183, 180, 177, 174, 171, 169, 166, 163, 160, 158, 155, 153, 151, 149, 147, 145, 144, 143, 142, 141, 140, 140, 140, 140, 140, 140, 141, 142, 143, 144, 145, 147, 149, 151, 153, 155, 158, 160, 163, 166, 169, 171, 174, 177, 180, 183, 186, 189, 192, 195, 197, 200, 202, 205, 207, 209, 211, 213, 214, 216, 217, 218, 219, 219, 219, 219, 219, 219, 219, 218, 217, 216, 214, 213, 211, 209, 207, 205, 202, 200, 197, 195, 192, 189, 186, 183, 180, 177, 174, 171, 169, 166, 163, 160, 158, 155, 153, 151, 149, 147, 145, 144, 143, 142, 141, 140, 140, 140, 140, 140, 140, 141, 142, 143, 144, 145, 147, 149, 151, 153, 155, 158, 160, 163, 166, 169, 171, 174, 177, 180, 183, 186, 189, 192, 195, 197, 200, 202, 205, 207, 209, 211, 213, 214, 216, 217, 218, 219, 219, 219
 
align 256
sin_x1 BYTE 184, 190, 196, 202, 208, 213, 219, 224, 228, 232, 235, 238, 241, 242, 243, 243, 243, 242, 241, 238, 235, 232, 228, 224, 219, 214, 208, 202, 196, 190, 184, 177, 171, 165, 159, 154, 148, 143, 139, 135, 132, 129, 126, 125, 124, 124, 124, 125, 126, 129, 132, 135, 139, 143, 148, 153, 159, 165, 171, 177, 183, 190, 196, 202, 208, 213, 219, 224, 228, 232, 235, 238, 241, 242, 243, 243, 243, 242, 241, 238, 235, 232, 228, 224, 219, 214, 208, 202, 196, 190, 184, 177, 171, 165, 159, 154, 148, 143, 139, 135, 132, 129, 126, 125, 124, 124, 124, 125, 126, 129, 132, 135, 139, 143, 148, 153, 159, 165, 171, 177,0,0,0

 BYTE 0,0,0,0,0,0,0,0



current_msg byte 0

message_table
        byte <message1
        byte >message1
        byte <message2
        byte >message2
        byte <message1
        byte >message1
        byte <message3
        byte >message3
        byte <message1
        byte >message1
        byte <message4
        byte >message4
        byte <message1
        byte >message1
        byte <message5
        byte >message5
        byte <message1
        byte >message1
        byte <message6
        byte >message6
        byte <message1
        byte >message1
        byte <message7
        byte >message7
        byte <message1
        byte >message1
        byte <message8
        byte >message8
        byte <message1
        byte >message1

        byte 00,00

fade_out_colors  byte 0,11,6,14,3,15,1,1
fade_in_colors   byte 1,1,15,3,14,6,11,0,0,0,0,0,0

;title_fade1     byte 12, 11, 4, 6, 14, 3, 1, 1, 1, 3, 14, 6, 4, 11, 12, 12
;                byte 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12
;                byte 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12
;                byte 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12                        

;title_fade2     byte 12, 12, 12, 12, 12, 12, 12, 12, 11, 4, 6, 14, 3, 1, 1, 1
;                byte 3, 14, 6, 4, 11, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12
;                byte 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12
;                byte 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12                
;                 

title_fade1     byte 1, 11, 4, 6, 14, 3, 1, 1, 1, 3, 14, 6, 4, 11, 4, 4
                byte 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4
                byte 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4
                byte 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4                        

title_fade2     byte 4, 4, 4, 4, 4, 4, 11, 4, 6, 14, 3, 1, 1, 1
                byte 3, 14, 6, 4, 11, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4
                byte 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4
                byte 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4                



message1        text '                '
                byte $ff


message2        text '    atlantis    '
                byte $ff

message3        text '      f4cg      '
                byte $ff

message4        text '       '
                byte 27, 28
                text '       '
                byte $ff

message5        text '    mario64     '
                byte $ff

message6        text '    presents    '
                byte $ff

message7        text '   endstation   '
                byte $ff

message8        text ' enjoy the ride '
                byte $ff

message9        text '                '
                byte $ff



;message2
;        text '    you are     '
;        ;byte 0,0,0,0,0,0,27,28,29,30,31,0,0,0,0,0,0
;        byte $ff



fade_ctr_t byte $0

lsrl_buf BYTE 0   

lsrl_table1

        BYTE 00,00,00,01,01,01,02,02,03,04,05,06,06,07,07,07
        BYTE 06,06,06,05,04,03,02,02,01,01,01,$ff


        BYTE 00,01,02,03,04,05,06,07, 00,01,02,03,04,05,06,07
        BYTE 00,01,02,03,04,05,06,07, 00,01,02,03,04,05,06,07


        BYTE 07,06,05,04,03,02,01,00, 07,06,05,04,03,02,01,00
        BYTE 07,06,05,04,03,02,01,00, 07,06,05,04,03,02,01,00

        BYTE $ff

lsrl_table2
        BYTE 08,08,08,08,08,08,08,08, 09,09,09,09,09,09,09,09
        BYTE 10,10,10,10,10,10,10,10, 11,11,11,11,11,11,11,11

        BYTE 11,11,11,11,11,11,11,11, 10,10,10,10,10,10,10,10
        BYTE 09,09,09,09,09,09,09,09, 08,08,08,08,08,08,08,08

        BYTE $ff

lsr_ctr BYTE 0

LOGO_X  BYTE $06

DRL_ani_ctr BYTE $ff

DRL_ANI_TBL
        BYTE <M64LOGO_color0
        BYTE >M64LOGO_color0
        BYTE <M64LOGO_color1
        BYTE >M64LOGO_color1
        BYTE <M64LOGO_color2
        BYTE >M64LOGO_color2
        BYTE <M64LOGO_color3
        BYTE >M64LOGO_color3
        BYTE <M64LOGO_color4
        BYTE >M64LOGO_color4
        BYTE <M64LOGO_color5
        BYTE >M64LOGO_color5
        BYTE <M64LOGO_color6
        BYTE >M64LOGO_color6
        BYTE <M64LOGO_color7
        BYTE >M64LOGO_color7




bin2hextbl
        BYTE '0123456789abcdef'


last1 BYTE 0
last2 BYTE 0
last3 BYTE 0

DO1   BYTE 0
DO2   BYTE 0
DO3   BYTE 0





align 256
rb_colors1
 BYTE 4, 14, 15, 3, 1, 1, 3, 15, 14, 4
 BYTE 0
 BYTE 6, 14, 15, 1, 15, 14, 6
 BYTE 0
 BYTE 6, 14, 1, 14, 6,0 

 BYTE 4, 14, 15, 1, 1, 1, 1, 15, 14, 4
 BYTE 0
 BYTE 6, 14, 15, 1, 15, 14, 6
 BYTE 0
 BYTE 6, 14, 1, 14, 6,0 

 BYTE 4, 14, 1, 1, 1, 1, 1, 1, 14, 4
 BYTE 0
 BYTE 6, 14, 1, 1, 1, 14, 6
 BYTE 0
 BYTE 6, 1,  1, 6,0 ,0

 BYTE 4, 1, 1, 1, 1, 1, 1, 1, 1, 4
 BYTE 0
 BYTE 6, 1, 1, 1, 1, 1, 6
 BYTE 0
 BYTE 6, 1, 6, 0 ,0,0

 BYTE 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0 

 BYTE 4, 1, 1, 1, 1, 1, 1, 1, 1, 4
 BYTE 0
 BYTE 6, 1, 1, 1, 1, 1, 6
 BYTE 0
 BYTE 6, 1, 6, 0,0 ,0 

 BYTE 4, 14, 1, 1, 1, 1, 1, 1, 14, 4
 BYTE 0
 BYTE 6, 14, 1, 1, 1, 14, 6
 BYTE 0
 BYTE 6, 1, 1, 6, 0,0 

 BYTE 4, 14, 15, 1, 1, 1, 1, 15, 14, 4
 BYTE 0
 BYTE 6, 14, 15, 1, 15, 14, 6
 BYTE 0
 BYTE 6, 14, 1, 14, 6,0 


 BYTE 4, 14, 15, 3, 1, 1, 3, 15, 14, 4
 BYTE 0
 BYTE 6, 14, 15, 1, 15, 14, 6
 BYTE 0
 BYTE 6, 14, 1, 14, 6,0 

 BYTE 4, 14, 15, 3, 1, 1, 3, 15, 14, 4
 BYTE 0
 BYTE 6, 14, 15, 1, 15, 14, 6
 BYTE 0
 BYTE 6, 14, 1, 14, 6,0 


 
 BYTE 4, 14, 15, 3, 1, 1, 3, 15, 14, 4
 BYTE 0
 BYTE 6, 14, 15, 1, 15, 14, 6
 BYTE 0
 BYTE 6, 14, 1, 14, 6,0 

 BYTE 4, 14, 15, 3, 1, 1, 3, 15, 14, 4
 BYTE 0
 BYTE 6, 14, 15, 1, 15, 14, 6
 BYTE 0
 BYTE 6, 14, 1, 14, 6,0 

 BYTE 4, 14, 15, 3, 1, 1, 3, 15, 14, 4
 BYTE 0
 BYTE 6, 14, 15, 1, 15, 14, 6
 BYTE 0
 BYTE 6, 14, 1, 14, 6,0 

 BYTE 4, 14, 15, 3, 1, 1, 3, 15, 14, 4
 BYTE 0
 BYTE 6, 14, 15, 1, 15, 14, 6
 BYTE 0
 BYTE 6, 14, 1, 14, 6,0 

 BYTE 4, 14, 15, 3, 1, 1, 3, 15, 14, 4
 BYTE 0
 BYTE 6, 14, 15, 1, 15, 14, 6
 BYTE 0
 BYTE 6, 14, 1, 14, 6,0 

 BYTE 4, 14, 15, 3, 1, 1, 3, 15, 14, 4
 BYTE 0
 BYTE 6, 14, 15, 1, 15, 14, 6
 BYTE 0
 BYTE 6, 14, 1, 14, 6,0 

 BYTE 4, 14, 15, 3, 1, 1, 3, 15, 14, 4
 BYTE 0
 BYTE 6, 14, 15, 1, 15, 14, 6
 BYTE 0
 BYTE 6, 14, 1, 14, 6,0 

 BYTE 4, 14, 15, 3, 1, 1, 3, 15, 14, 4
 BYTE 0
 BYTE 6, 14, 15, 1, 15, 14, 6
 BYTE 0
 BYTE 6, 14, 1, 14, 6,0 

 BYTE 4, 14, 15, 3, 1, 1, 3, 15, 14, 4
 BYTE 0
 BYTE 6, 14, 15, 1, 15, 14, 6
 BYTE 0
 BYTE 6, 14, 1, 14, 6,0 

 BYTE 4, 14, 15, 3, 1, 1, 3, 15, 14, 4
 BYTE 0
 BYTE 6, 14, 15, 1, 15, 14, 6
 BYTE 0
 BYTE 6, 14, 1, 14, 6,0 

 BYTE 4, 14, 15, 3, 1, 1, 3, 15, 14, 4
 BYTE 0
 BYTE 6, 14, 15, 1, 15, 14, 6
 BYTE 0
 BYTE 6, 14, 1, 14, 6,0 

align 256
rb_colors2

 byte 0, 0, 0, 0
 BYTE 6, 14, 1, 14, 6, 0
 BYTE 6, 14, 15, 1, 15, 14, 6, 0
 BYTE 4, 14, 15, 3, 1
 BYTE 1, 3, 15, 14, 4, 0

 byte 6, 0, 0, 0
 BYTE 6, 14, 1, 14, 6, 0
 BYTE 6, 14, 15, 1, 15, 14, 6, 0
 BYTE 4, 14, 15, 3, 1
 BYTE 1, 3, 15, 14, 4, 0

 byte 6, 0, 0, 0
 BYTE 6, 14, 1, 14, 6, 0
 BYTE 6, 14, 15, 1, 15, 14, 6, 0
 BYTE 4, 14, 15, 3, 1
 BYTE 1, 3, 15, 14, 4, 0

 byte 6, 0, 0, 0
 BYTE 6, 14, 1, 14, 6, 0
 BYTE 6, 14, 15, 1, 15, 14, 6, 0
 BYTE 4, 14, 15, 3, 1
 BYTE 1, 3, 15, 14, 4, 0

 byte 6, 0, 0, 0
 BYTE 6, 14, 1, 14, 6, 0
 BYTE 6, 14, 15, 1, 15, 14, 6, 0
 BYTE 4, 14, 15, 3, 1
 BYTE 1, 3, 15, 14, 4, 0

 byte 6, 0, 0, 0
 BYTE 6, 14, 1, 14, 6, 0
 BYTE 6, 14, 15, 1, 15, 14, 6, 0
 BYTE 4, 14, 15, 3, 1
 BYTE 1, 3, 15, 14, 4, 0

 byte 6, 0, 0, 0
 BYTE 6, 14, 1, 14, 6, 0
 BYTE 6, 14, 15, 1, 15, 14, 6, 0
 BYTE 4, 14, 15, 3, 1
 BYTE 1, 3, 15, 14, 4, 0

 byte 6, 0, 0, 0
 BYTE 6, 14, 1, 14, 6, 0
 BYTE 6, 14, 15, 1, 15, 14, 6, 0
 BYTE 4, 14, 15, 3, 1
 BYTE 1, 3, 15, 14, 4, 0

 byte 6, 0, 0, 0
 BYTE 6, 14, 1, 14, 6, 0
 BYTE 6, 14, 15, 1, 15, 14, 6, 0
 BYTE 4, 14, 15, 3, 1
 BYTE 1, 3, 15, 14, 4, 0

 byte 6, 0, 0, 0
 BYTE 6, 14, 1, 14, 6, 0
 BYTE 6, 14, 15, 1, 15, 14, 6, 0
 BYTE 4, 14, 15, 3, 1
 BYTE 1, 3, 15, 14, 4, 0

 byte 6, 0, 0, 0
 BYTE 6, 14, 1, 14, 6, 0
 BYTE 6, 14, 15, 1, 15, 14, 6, 0
 BYTE 4, 14, 15, 3, 1
 BYTE 1, 3, 15, 14, 4, 0

 byte 6, 0, 0, 0
 BYTE 6, 14, 1, 14, 6, 0
 BYTE 6, 14, 15, 1, 15, 14, 6, 0
 BYTE 4, 14, 15, 3, 1
 BYTE 1, 3, 15, 14, 4, 0

 byte 6, 0, 0, 0
 BYTE 6, 14, 1, 14, 6, 0
 BYTE 6, 14, 15, 1, 15, 14, 6, 0
 BYTE 4, 14, 15, 3, 1
 BYTE 1, 3, 15, 14, 4, 0

 byte 6, 0, 0, 0
 BYTE 6, 14, 1, 14, 6, 0
 BYTE 6, 14, 15, 1, 15, 14, 6, 0
 BYTE 4, 14, 15, 3, 1
 BYTE 1, 3, 15, 14, 4, 0

 byte 6, 0, 0, 0
 BYTE 6, 14, 1, 14, 6, 0
 BYTE 6, 14, 15, 1, 15, 14, 6, 0
 BYTE 4, 14, 15, 3, 1
 BYTE 1, 3, 15, 14, 4, 0

 byte 6, 0, 0, 0
 BYTE 6, 14, 1, 14, 6, 0
 BYTE 6, 14, 15, 1, 15, 14, 6, 0
 BYTE 4, 14, 15, 3, 1
 BYTE 1, 3, 15, 14, 4, 0

 byte 6, 0, 0, 0
 BYTE 6, 14, 1, 14, 6, 0
 BYTE 6, 14, 15, 1, 15, 14, 6, 0
 BYTE 4, 14, 15, 3, 1
 BYTE 1, 3, 15, 14, 4, 0

 byte 6, 0, 0, 0
 BYTE 6, 14, 1, 14, 6, 0
 BYTE 6, 14, 15, 1, 15, 14, 6, 0
 BYTE 4, 14, 15, 3, 1
 BYTE 1, 3, 15, 14, 4, 0


a4_colors                 
; BYTE 6, 14, 1, 14, 6, 0
; BYTE 6, 14, 15, 1, 15, 14, 6, 0
; BYTE 4, 14, 15, 3, 1
; BYTE 1, 3, 15, 14, 4, 0
; BYTE 6, 14, 1, 14, 6, 0
; BYTE 6, 14, 15, 1, 15, 14, 6, 0
; BYTE 4, 14, 15, 3, 1
; BYTE 1, 3, 15, 14, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
; BYTE 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
; BYTE 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0


 BYTE 12, 5, 13, 1, 13, 5, 12
 BYTE 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
 BYTE 12, 6, 14, 1, 14, 6, 12
 BYTE 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
 BYTE 12, 3, 10, 1, 10, 3, 12
 BYTE 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0  


; -------------------------------------------------------
; startup parts


do_fadein       ldx #50

do_fadein3
                lda #255
                cmp $d012
                bne *-3

                lda #250
                cmp $d012
                bne *-3

            

                dex
                bne do_fadein3

                


do_fadein2     

                lda #255
                cmp $d012
                bne *-3

                lda #250
                cmp $d012
                bne *-3

                lda #245
                cmp $d012
                bne *-3

                


                jsr strt_draw_logo

                inc fi_frmctr
                lda fi_frmctr
                cmp #18
                beq @endme

                jmp do_fadein2

@endme          



                jsr fi_msg

@endmeloop      jsr fi_msg1

                inc fi_frmctr
                lda fi_frmctr
                cmp #30
                beq @endme2

                lda #255
                cmp $d012
                bne *-3

                lda #250
                cmp $d012
                bne *-3

                lda #245
                cmp $d012
                bne *-3

                lda #240
                cmp $d012
                bne *-3

                lda #235
                cmp $d012
                bne *-3

               

                jmp @endmeloop
                

@endme2         

; fade in sprite a4

        ldx #$98
        stx $07f8
        lda #%00000001
        sta $d015 ; 1-8 sprites ON
        lda #$0
        sta $d01c ; 1-8 sprites multicolor OFF
        
        ; X-COORDS
        lda #0
        sta $d010 ; hi bit x for all = 0

        ; X
        lda #181
        sta $d000
        lda #97
        sta $d001

@xxx1
        ldx fi_spr_ctr
        lda fade_in_colors_sprite,x
        sta $d027

        jsr rloop2

        inc fi_spr_ctr
        lda fi_spr_ctr
        cmp #10 ;#10
        bne @xxx1




                jmp continue
        

fi_frmctr       byte 0




strt_draw_logo
                lda #$04
                sta strt_drl_sta_0400+2
                lda Logo_X
                sta strt_drl_sta_0400+1

                lda #$d8
                sta strt_drl_sta_d800+2
                lda Logo_X
                sta strt_drl_sta_d800+1         


                ldx #0
        
strt_drl_loop_outer
                ldy #24

strt_drl_loop_inner
                lda M64LOGO_chars,x
                
strt_drl_sta_0400    sta $0400,x

strt_drl_lda_logo_color
                stx tmp_x
                
                lda M64LOGO_color,x
  
                ldx fi_frmctr                
@xxx1
                cmp #1 ; white
                beq fi_white
                cmp #15
                beq fi_lgry
                cmp #12
                beq fi_mgry
                cmp #14
                beq fi_lblu
                cmp #11
                beq fi_dgry

                jmp go2

fi_white        lda strt_fi_logo_col_white,x
                jmp go2

fi_lgry         lda strt_fi_logo_col_lgry,x
                jmp go2

fi_dgry         lda strt_fi_logo_col_dgry,x
                jmp go2

fi_lblu         lda strt_fi_logo_col_lblu,x
                jmp go2

fi_mgry         lda strt_fi_logo_col_mgry,x
                ;jmp go2

go2             ldx tmp_x



strt_drl_sta_d800    
                sta $d800,x

                inx                

                dey
                bne strt_drl_loop_inner

                cpx #192
                beq strt_drl_end

                lda strt_drl_sta_0400+1
        clc
                adc #16
                bne @strt_goon
                inc strt_drl_sta_0400+2
@strt_goon      sta strt_drl_sta_0400+1

                lda strt_drl_sta_d800+1
        clc
                adc #16
                bne @strt_goon2
                inc strt_drl_sta_d800+2
@strt_goon2     sta strt_drl_sta_d800+1


                jmp strt_drl_loop_outer

strt_drl_end 

                rts
;----------


strt_fi_logo_col_white 
                byte 0,0,0,0,0,0,0,0
                BYTE 00,11,11,12,12,15,15,1    ,15,1


strt_fi_logo_col_lgry
                byte 0,0,0,0,0,0,0,0
                BYTE 00,11,11, 6, 6,12,12,15   ,12,15

strt_fi_logo_col_dgry
                byte 0,0,0,0,0,0,0,0
                BYTE 00,11,11, 4,12,15,12,11,    4, 11

strt_fi_logo_col_lblu 
                byte 0,0,0,0,0,0,0,0
                BYTE 00,11,11, 6, 6,14, 3, 1,    3, 14

strt_fi_logo_col_mgry 
                byte 0,0,0,0,0,0,0,0
                BYTE 00,11,11,12,12,15,15, 3,   15,12

strt_fi_logo_ctr
                BYTE 0

tmp_x           BYTE 0

;__________


fi_msg
        ldx #0

@printloop
        lda #0
        sta $da68,x
        lda startmsg,x
        cmp #$ff
        beq @endprint
        ;cmp #32
        ;bne @dosta
        ;lda #0
        
;@dosta

        sta $668,x
        ;sta $700-35,x
        inx
        jmp @printloop
        
@endprint
        rts



fi_msg1 

        inc @fade_ctr
        ldx @fade_ctr
        
        lda fade_in_colors_strt,x

@goon1  
        ldx #16
@loop1
        sta $da68-1,x
        dex
        bne @loop1

;
@endme


        rts

@fade_ctr byte 0

@fade_in_out byte 1 ; 1 in, 0 out

@fade_stay_ctr byte 0
        
@delay byte 0

fade_in_colors_strt   byte 0,0,11,2,10,1,10,15,12,11,5,13,1

fade_in_colors_sprite byte 0, 11, 12, 15, 1, 15, 12, 15, 1, 1, 3, 11, 13
fi_spr_ctr byte 0

rloop2  
        ldy #2
@ll1
        ldx #$ff
        dex
        bne *-1

        lda #0
        cmp $d012
        bne *-3

        ldx #$ff
        dex
        bne *-1

        dey
        bne @ll1
        rts

