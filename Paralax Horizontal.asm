; Horizontal Paralax Scrolling...
;
; BF 2017

; Antic

DLPTR	EQU 560	
VDLIST	EQU $200	 
NMIEN	EQU $D40E
WSYNC	EQU $D40A
VCOUNT	EQU $D40B
RTCLK	EQU $14
SDMCTL	EQU 559

; Zeropage

ZP		equ $e0
zp2		equ $e2
zp3		equ $e4
zp4		equ $e6
zp5		equ	$e8
zp6		equ $ea
zp7		equ $ec
zp8		equ $ee

; Colors

COLPF0	EQU 708  
COLPF1	EQU 709 
COLPF2	EQU 710
COLPF3	EQU 711
COLBAK	EQU 712

COLPF0S	EQU $D016 
COLPF1S	EQU $D017
COLPF2S	EQU $D018
COLPF3S	EQU $D019
COLBAKS	EQU $D01A

;
; Start
;

	org $a800
	
	jsr refreshscreen
	jsr initscreen
	jsr draw

	ldy #<shr	;Charakter scroll routine, start imediate VBI
	ldx #>shr
	lda #6
	jsr $e45c

l
	jmp l
	
; 
; Shift right
; 
; This shifts the contents of char c1 to the right, into char c2
; All Bits pushed out of c2 will be set in the lsb of c1, thus the
; bits loop thru => c1 => c2 => c1.
;
; If we scroll the playfield to the left, c1 and c2 will appear to be
; not moving, because, every time pf is scrolled one pixel to the left
; c1 and c2 will be scrolled one pixel to the oposide direction.
;
;

c11	.byte 0
c22	.byte 0
wait
	.byte 3

shr
	pha				; Save registers
	txa
	pha
	tya
	pha

	dec wait
	bne out 
	
	ldx #0
	ldy #8
l11
	lda c1,x
	and #%00000011
	asl
	asl
	asl
	asl
	asl
	asl
	sta c11
	
	lda c2,x
	and #%00000011
	asl
	asl
	asl
	asl
	asl
	asl
	sta c22
	
	lda c1,x
	lsr 
	lsr
	ora c22
	sta c1,x
	
	lda c2,x
	lsr
	lsr
	ora c11
	sta c2,x
	
	inx
	dey
	bne l11
	
	lda #3
	sta wait
out	
	pla				; Get registers back
	tay
	pla
	tax
	pla
	
	jmp $e45f		; Leave imediate VBI

;
; Draw background
;

draw
	pha				; Save registers
	txa
	pha
	tya
	pha
	
	ldx #0
getlen
	lda 53770
	cmp #18			; a>18?
	bcs getlen		; yes!
	cmp #14 		; no. a<14?
	bcc getlen  	; no!
	
	tay
	lda #3
drw
	jsr plot
	iny			
	cpy #18
	bne drw		; Draw until we reach bottom of playfield
	inx
	cpx #184	; Draw until we reach end of playfield (right border)
	bne getlen	; Next  building
	
	pla				; Get registers back
	tay
	pla
	tax
	pla
	
	rts

;
; Plot 
;
; Writes any character you want, at any position in screen ram
;
; x-reg	= xpos
; y-reg	= ypos
; a		= Char
;
; Zeropage: zp7

zeichen	.byte 0
xr4		.byte 0
yr4		.byte 0

plot	
	stx xr4		; Save registers
	sty yr4
	
	sta zeichen		; Save character
	lda #<screen	; Init pointer at screen ram
	sta zp7		
	lda #>screen
	sta zp7+1
	
	cpy #0		; ypos equals 0, that is easy, jump
	beq set		; to our set- routine, we don't have to calcualte line- adress
p1	
	lda zp7		; Get pointer at screen ram
	clc			; Now calculate adress of line from given y- pos
	adc #bytes	; The slow way :-)
	sta zp7				
	lda zp7+1
	adc #0
	sta zp7+1
	dey
	bne p1
set
	txa
	tay
	lda zeichen
	sta (zp7),y
	
	ldx xr4		; Get registers back
	ldy yr4
	
	rts	
	
;
; Init Screen
;

initscreen
	lda #chset/256
	sta 756
	
	lda #200		; Color for bit combination: 10
	sta colpf1s									
	
	lda #14			; Color for bit combination: 11
	sta colpf2s
	
	lda #10			; Color for bit combination: 01	
	sta colpf0s									

	lda #24			; Color for bit combination=color 5 bit combination 11 (reverse character)
	sta colpf3s	
	
	lda #<dlgame	; Show playfield		
	sta dlptr						
	lda #>dlgame
	sta dlptr+1
	
	lda #<dli   	; Display- List- Interrupt on
	sta vdlist
	lda #>dli
	sta vdlist+1
	lda #$C0
	sta NMIEN
	
	rts
	
;
; Reset Playfield= Begin scrolling at screen 1
; To do this, we change the screen- ram adresses in our display list
; line by line by replacing them with our in "adtab" saved adresses
; 

refreshscreen
	pha				; Save registers
	txa
	pha
	tya
	pha

	ldx #19			; 20 rows
	ldy #0
lll0
	lda adtab+1,y	; Get adress from table
	sta z0+1,y		; Put it into lms of antic program => low byte
	iny
	lda adtab+1,y	; Same for high byte
	sta z0+1,y
	iny
	iny
	dex
	bne lll0		; All rows done?

	
	pla				; Get registers back
	tay
	pla
	tax
	pla
	
	rts
	
;
; Wait
; Sometimes our code is to fast........
;

wtt
	pha				; Save registers
	txa
	pha
	tya
	pha

	ldx #150
ww0	
	ldy #250
ww1	
	dey
	bne ww1
	dex
	bne ww0
	
	pla				; Get registers back
	tay
	pla
	tax
	pla
	
	rts

;
; DLI
;

dli
	pha				;  Save registers
	txa
	pha
	tya
	pha
	
	lda vcount
	asl
	cmp #38
	bcs dli1		; Is electron beam at row > 38?

	;
	; Set Chset and colors for score display
	;
	
	lda #>chset		; No! Electron beam is at line # below 38
	sta $d409		; so we are still within our score display
	lda #5			; Change colors for capital charcters (basic-)mode 1 or 2 	
	sta wsync
	sta colpf0s					
	lda #20 		; Change colors for lower case characters in (basic)mode 1,2 or 0
	sta colpf1s
	lda #116
	sta colpf2s
	lda #155
	sta colpf3s
	lda #14			; Bright white for the background
	sta colbaks 	; End of screen area for score display

dli1								
	lda vcount		; Is electron beam at row # bigger than 38?
	asl				; No? So we are still in area of score display
	cmp #38			; 
	bcc dlout		; Do nothing!

	;
	; Set chset for playfield and playfield colors
	;
								
	lda #>chset 	; Electron beam has crossed row 38 that means
	sta $d409		; we are in playfild area of our screen

	lda #200		; Color for bit combination: 10
	sta colpf1s									
	sta wsync
	
	lda #14			; Color for bit combination: 11
	sta colpf2s
	sta wsync
	
	lda #10			; Color for bit combination: 01	
	sta colpf0s									
	sta wsync

	lda #24			; Color for bit combination=color 5 bit combination 11 (reverse character)
	sta colpf3s		

	; Draw sky

	ldx #15			; Draw Background
	lda #127		; Bright blue
dd1
	sta wsync		; Wait until scanline is finished
	sta colbaks 	; Init background color reg.
	ldy #75			; This determins the height of each color cycle
dd2
	dey
	bne dd2
	
	sec				; Blue get's darker
	sbc #1
	dex				; Until we reach the lower third
	bne dd1			; of our playfield

	; Draw ground

	ldx #15			; Draw ground
	lda #194		; Start with dark green
dc1
	sta wsync
	sta colbaks
	ldy #19
dc2
	dey				; Height of each color cycle
	bne dc2
	clc
	adc #1
	dex
	bne dc1
dlout
	pla				; Get registers back
	tay
	pla
	tax
	pla
	
	rti
	
;	
; Antic program 
;

bytes	equ 246							; # of bytes (width of "playfield")

gr0		equ $02							; Gr. 0
gr1		equ $06							; Gr. 1
gr2		equ $07
gr12	equ $14							; Gr. 12, horiz. scrolling enabeled

dlgame						 	
	.byte 112+128						; Start of Antic programm for our playfield			
	.byte 112
	
	.byte $40+gr1,a(scorelin)			; Message line
	.byte 112

	.byte $70+128						; 8 empty lines, start display- list interrupt
	
z0	.byte $40+gr12,a(screen)			; Playfield row 0
z1	.byte $40+gr12,a(screen+1*bytes) 	
z2	.byte $40+gr12,a(screen+2*bytes) 
z3	.byte $40+gr12,a(screen+3*bytes) 
z4	.byte $40+gr12,a(screen+4*bytes)
z5	.byte $40+gr12,a(screen+5*bytes)
z6	.byte $40+gr12,a(screen+6*bytes)
z7	.byte $40+gr12,a(screen+7*bytes)
z8	.byte $40+gr12,a(screen+8*bytes)
z9	.byte $40+gr12,a(screen+9*bytes)
z10	.byte $40+gr12,a(screen+10*bytes)
z11	.byte $40+gr12,a(screen+11*bytes)
z12	.byte $40+gr12,a(screen+12*bytes)
z13	.byte $40+gr12,a(screen+13*bytes)
z14	.byte $40+gr12,a(screen+14*bytes)
z15	.byte $40+gr12,a(screen+15*bytes)
z16	.byte $40+gr12,a(screen+16*bytes) 
z17	.byte $40+gr12,a(screen+17*bytes)
z18	.byte $40+gr12,a(screen+18*bytes)
z19	.byte $40+gr12,a(screen+19*bytes)	; Row 20

	.byte $41,a(dlgame)				 	; End of display- list, start all over again....
	
scorelin								; Contents of screen ram for status display
	.byte "PaRaLaX             "
	
; Adress table

dummy	equ 0						; a, well, a dummy.....
	
adtab
	.byte dummy,a(screen)			; Row 1
	.byte dummy,a(screen+1*bytes)	; Now we see why there is this suspicous dummy,
	.byte dummy,a(screen+2*bytes)	; It is there to let 'adtab' look exactly like 
	.byte dummy,a(screen+3*bytes)	; the structure of our antic program.
	.byte dummy,a(screen+4*bytes)	; In our antic programm dummy contains the 'lsm'
	.byte dummy,a(screen+5*bytes)	; instruction
	.byte dummy,a(screen+6*bytes)
	.byte dummy,a(screen+7*bytes)
	.byte dummy,a(screen+8*bytes)
	.byte dummy,a(screen+9*bytes)
	.byte dummy,a(screen+10*bytes)
	.byte dummy,a(screen+11*bytes)
	.byte dummy,a(screen+12*bytes)
	.byte dummy,a(screen+13*bytes)
	.byte dummy,a(screen+14*bytes)
	.byte dummy,a(screen+15*bytes)
	.byte dummy,a(screen+16*bytes)
	.byte dummy,a(screen+17*bytes)
	.byte dummy,a(screen+18*bytes)
	.byte dummy,a(screen+19*bytes)	; Row 20

;
; "Playfield" 
;

screen
:246	.byte 0
:246/2	.byte 2,1						; 246 bytes wide, 20 rows height=4920 bytes screen ram...
:246/2	.byte 1,2
:246/2	.byte 2,1						; 246 bytes wide, 20 rows height=4920 bytes screen ram...
:246/2	.byte 1,2
:246/2	.byte 2,1						; 246 bytes wide, 20 rows height=4920 bytes screen ram...
:246/2	.byte 1,2
:246/2	.byte 2,1						; 246 bytes wide, 20 rows height=4920 bytes screen ram...
:246/2	.byte 1,2
:246/2	.byte 2,1						; 246 bytes wide, 20 rows height=4920 bytes screen ram...
:246/2	.byte 1,2
:246/2	.byte 2,1						; 246 bytes wide, 20 rows height=4920 bytes screen ram...
:246/2	.byte 1,2
:246/2	.byte 2,1						; 246 bytes wide, 20 rows height=4920 bytes screen ram...
:246/2	.byte 1,2
:246/2	.byte 2,1						; 246 bytes wide, 20 rows height=4920 bytes screen ram...
:246/2	.byte 1,2


; Charset
; Dump of:	ANTIK2.FNT

	org $4400
chset
	.byte 0,0,0,0,0,0,0,0
c1
	.byte 85	,	85	,	85	,	85	,	85	,	85	,	85	,	85
c2
:8	.byte $0

:8	.byte	$ff
	.byte 0									; Something odd?
	.byte	$3e,$60,$3c,$06,$7c,$18,$00,$00
	.byte	$66,$6c,$18,$30,$66,$46,$00,$1c
	.byte	$36,$1c,$38,$6f,$66,$3b,$00,$00
	.byte	$18,$18,$18,$00,$00,$00,$00,$06
	.byte	$0e,$1c,$18,$18,$1c,$0e,$06,$60
	.byte	$70,$38,$18,$18,$38,$70,$60,$00
	.byte	$66,$3c,$ff,$3c,$66,$00,$00,$00
	.byte	$18,$18,$7e,$18,$18,$00,$00,$00
	.byte	$00,$00,$00,$00,$18,$18,$30,$00
	.byte	$00,$00,$7e,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$18,$18,$00,$00
	.byte	$06,$0c,$18,$30,$60,$40,$00,$7c
	.byte	$ce,$c6,$c6,$c6,$e6,$7c,$00,$38
	.byte	$38,$18,$18,$18,$18,$18,$00,$7c
	.byte	$e6,$0c,$18,$30,$60,$fe,$00,$7e
	.byte	$0c,$18,$0c,$06,$66,$3c,$00,$0c
	.byte	$1c,$3c,$6c,$cc,$fe,$0c,$00,$7e
	.byte	$60,$7c,$06,$06,$66,$3c,$00,$7c
	.byte	$c6,$c0,$fc,$ce,$e6,$7c,$00,$7e
	.byte	$06,$0c,$18,$30,$30,$30,$00,$7c
	.byte	$ce,$e6,$7c,$ce,$e6,$7c,$00,$7c
	.byte	$ce,$c6,$e6,$7e,$0c,$18,$30,$00
	.byte	$38,$38,$00,$00,$38,$38,$00,$00
	.byte	$00,$18,$18,$00,$18,$18,$30,$06
	.byte	$0c,$18,$30,$18,$0c,$06,$00,$00
	.byte	$00,$7e,$00,$00,$7e,$00,$00,$60
	.byte	$30,$18,$0c,$18,$30,$60,$00,$3c
	.byte	$66,$66,$0c,$18,$00,$18,$00,$6a
	.byte	$c9,$00,$7e,$00,$93,$56,$00,$7a
	.byte	$9c,$34,$36,$3e,$66,$66,$c3,$ee
	.byte	$73,$6b,$6b,$7e,$6b,$6b,$de,$3d
	.byte	$66,$d4,$d0,$d0,$d0,$e6,$7c,$ee
	.byte	$73,$6b,$6b,$6b,$6b,$63,$de,$fe
	.byte	$66,$60,$78,$60,$63,$66,$7c,$fe
	.byte	$66,$60,$78,$60,$60,$68,$70,$3d
	.byte	$66,$d6,$d0,$de,$d6,$66,$3c,$c7
	.byte	$c6,$c6,$ce,$fe,$e6,$c6,$ce,$34
	.byte	$18,$18,$18,$18,$18,$1c,$2c,$7f
	.byte	$16,$16,$56,$06,$de,$7c,$78,$c6
	.byte	$6d,$6c,$78,$78,$6c,$6c,$c6,$e0
	.byte	$60,$60,$60,$61,$66,$7e,$78,$c6
	.byte	$ee,$fe,$d6,$c6,$c6,$c6,$c6,$c6
	.byte	$c6,$e6,$f6,$de,$ce,$c6,$c6,$7d
	.byte	$ce,$d6,$d6,$d6,$d6,$e6,$7c,$fc
	.byte	$66,$6e,$66,$6d,$60,$60,$c0,$7d
	.byte	$e6,$d6,$d6,$d6,$d6,$ce,$7f,$dd
	.byte	$66,$6e,$66,$7c,$6c,$66,$e3,$ba
	.byte	$66,$60,$3c,$06,$46,$66,$5c,$fe
	.byte	$30,$68,$68,$62,$6c,$74,$3c,$e6
	.byte	$66,$66,$66,$66,$6e,$6e,$3f,$c3
	.byte	$66,$66,$66,$66,$66,$3d,$18,$e3
	.byte	$c3,$c3,$d3,$cb,$df,$77,$62,$c0
	.byte	$c3,$66,$3c,$3c,$66,$c3,$03,$c3
	.byte	$66,$66,$3c,$5a,$18,$18,$18,$7e
	.byte	$c6,$0c,$38,$7c,$60,$c3,$fe,$00
	.byte	$1e,$18,$18,$18,$18,$1e,$00,$00
	.byte	$40,$60,$30,$18,$0c,$06,$00,$00
	.byte	$78,$18,$18,$18,$18,$78,$00,$00
	.byte	$08,$1c,$36,$63,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$ff,$00,$00
	.byte	$36,$7f,$7f,$3e,$1c,$08,$00,$18
	.byte	$18,$0c,$07,$07,$0c,$18,$18,$03
	.byte	$03,$03,$03,$03,$03,$03,$03,$18
	.byte	$18,$38,$f0,$e0,$00,$00,$00,$18
	.byte	$18,$30,$e0,$e0,$30,$18,$18,$00
	.byte	$00,$00,$c0,$e0,$30,$18,$18,$03
	.byte	$07,$0e,$1c,$38,$70,$e0,$c0,$c0
	.byte	$e0,$70,$38,$1c,$0e,$07,$03,$01
	.byte	$03,$07,$0f,$1f,$3f,$7f,$ff,$00
	.byte	$00,$00,$00,$0f,$0f,$0f,$0f,$80
	.byte	$c0,$e0,$f0,$f8,$fc,$fe,$ff,$0f
	.byte	$0f,$0f,$0f,$00,$00,$00,$00,$f0
	.byte	$f0,$f0,$f0,$00,$00,$00,$00,$ff
	.byte	$ff,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$ff,$ff,$00
	.byte	$00,$00,$00,$f0,$f0,$f0,$f0,$00
	.byte	$00,$1c,$1c,$77,$77,$08,$1c,$00
	.byte	$00,$00,$03,$07,$0c,$18,$18,$00
	.byte	$00,$00,$ff,$ff,$00,$00,$00,$18
	.byte	$18,$18,$ff,$ff,$18,$18,$18,$00
	.byte	$00,$3c,$7e,$7e,$7e,$3c,$00,$00
	.byte	$00,$00,$00,$ff,$ff,$ff,$ff,$c0
	.byte	$c0,$c0,$c0,$c0,$c0,$c0,$c0,$00
	.byte	$00,$00,$c3,$e7,$3c,$18,$18,$18
	.byte	$18,$3c,$e7,$c3,$00,$00,$00,$f0
	.byte	$f0,$f0,$f0,$f0,$f0,$f0,$f0,$18
	.byte	$18,$0c,$07,$03,$00,$00,$00,$78
	.byte	$60,$78,$60,$7e,$18,$1e,$00,$00
	.byte	$18,$3c,$7e,$18,$18,$18,$00,$00
	.byte	$18,$18,$18,$7e,$3c,$18,$00,$00
	.byte	$18,$30,$7e,$30,$18,$00,$00,$00
	.byte	$18,$0c,$7e,$0c,$18,$00,$00,$18
	.byte	$3c,$66,$c3,$18,$3c,$66,$c3,$00
	.byte	$40,$3e,$66,$66,$66,$3b,$40,$30
	.byte	$60,$62,$7c,$66,$66,$7c,$02,$00
	.byte	$40,$3c,$76,$60,$66,$3c,$40,$0c
	.byte	$06,$46,$3e,$66,$66,$3a,$40,$00
	.byte	$40,$3c,$66,$7e,$60,$38,$04,$2e
	.byte	$18,$18,$18,$3e,$18,$18,$00,$00
	.byte	$86,$7c,$cc,$cc,$78,$c2,$7c,$c0
	.byte	$60,$62,$6c,$76,$66,$66,$00,$30
	.byte	$00,$18,$0c,$0c,$0c,$0e,$00,$0c
	.byte	$00,$0c,$0c,$0c,$0c,$18,$38,$00
	.byte	$c0,$66,$6c,$78,$6c,$e6,$00,$30
	.byte	$18,$18,$18,$18,$18,$0c,$00,$00
	.byte	$c0,$66,$7e,$7e,$6a,$63,$00,$00
	.byte	$e2,$7c,$66,$66,$66,$63,$00,$00
	.byte	$40,$3c,$6e,$66,$76,$3c,$40,$00
	.byte	$c2,$7c,$66,$66,$7c,$62,$e0,$00
	.byte	$40,$3b,$66,$66,$3e,$46,$07,$00
	.byte	$e0,$7c,$76,$60,$60,$60,$00,$00
	.byte	$40,$3e,$70,$3c,$8e,$7c,$02,$30
	.byte	$30,$7e,$30,$30,$30,$36,$1c,$00
	.byte	$01,$e6,$66,$66,$6e,$3b,$00,$00
	.byte	$0d,$e6,$66,$66,$3c,$18,$00,$00
	.byte	$00,$e3,$6b,$7f,$3e,$36,$00,$00
	.byte	$c0,$66,$3c,$18,$3c,$66,$03,$00
	.byte	$00,$e6,$66,$66,$3e,$cc,$78,$00
	.byte	$00,$7e,$0c,$7e,$30,$7e,$00,$00
	.byte	$18,$3c,$7e,$7e,$18,$3c,$00,$18
	.byte	$18,$18,$18,$18,$18,$18,$18,$00
	.byte	$7e,$78,$7c,$6e,$66,$06,$00,$08
	.byte	$18,$38,$78,$38,$18,$08,$00,$00
	.byte	$00,$00,$ff,$7e,$3c,$18,$00,$01