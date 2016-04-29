#lang racket
(provide (all-defined-out))

;; registers

(define zero 'zero) ; constant 0
(define at 'at) ; reserved for assembler

(define v0 'v0) ; expression evaluation and results of a function
(define v1 'v1) ; expression evaluation and results of a function

(define a0 'a0) ; argument 1
(define a1 'a1) ; argument 2
(define a2 'a2) ; argument 3
(define a3 'a3) ; argument 4

(define t0 't0) ; temporary (not preserved accross call)
(define t1 't1) ; temporary (not preserved accross call)
(define t2 't2) ; temporary (not preserved accross call)
(define t3 't3) ; temporary (not preserved accross call)
(define t4 't4) ; temporary (not preserved accross call)
(define t5 't5) ; temporary (not preserved accross call)
(define t6 't6) ; temporary (not preserved accross call)
(define t7 't7) ; temporary (not preserved accross call)
(define t8 't8) ; temporary (not preserved accross call)
(define t9 't9) ; temporary (not preserved accross call)

(define s0 's0) ; saved temporary (preserved accross call)
(define s1 's1) ; saved temporary (preserved accross call)
(define s2 's2) ; saved temporary (preserved accross call)
(define s3 's3) ; saved temporary (preserved accross call)
(define s4 's4) ; saved temporary (preserved accross call)
(define s5 's5) ; saved temporary (preserved accross call)
(define s6 's6) ; saved temporary (preserved accross call)
(define s7 's7) ; saved temporary (preserved accross call)

(define k0 'k0) ; reserved for OS kernel
(define k1 'k1) ; reserved for OS kernel

(define gp 'gp) ; pointer to global area
(define sp 'sp) ; stack pointer
(define fp 'fp) ; frame pointer
(define ra 'ra) ; return address (used by function call)

;; opcode

(define abs     'abs) ; abs rdest, rsrc

(define add     'add)   ; add   rd, rs, rt
(define addi    'addi)  ; addi  rt, rs, imm
(define addiu   'addiu) ; addiu rt, rs, imm
(define addu    'addu)  ; addu  rd, rs, rt

(define op-and  'and)  ; and  rd, rs, rt
(define andi    'andi) ; andi rt, rs, imm

(define b       'b)      ; b      label
(define bclf    'bclf)   ; bclf   cc lael
(define bclt    'bclt)   ; bclt   cc lael
(define beq     'beq)    ; beq    rs, rt, label
(define beqz    'beqz)   ; beqz   rsrc, label
(define bge     'bge)    ; bge    rsrc1, rsrc2, label
(define bgeu    'bgeu)   ; bgeu   rsrc1, rsrc2, label
(define bgez    'bgez)   ; bgez   rs, label
(define bgezal  'bgezal) ; bgezal rs, label
(define bgt     'bgt)    ; bgt    rsrc1, src2, label
(define bgtu    'bgtu)   ; bgtu   rsrc1, src2, label
(define bgtz    'bgtz)   ; bgtz   rs, label
(define ble     'ble)    ; ble    rsrc1, src2, label
(define bleu    'bleu)   ; bleu   rsrc1, src2, label
(define blez    'blez)   ; blez   rs, label
(define blt     'blt)    ; blt    rsrc1, rsrc2, label
(define bltu    'bltu)   ; bltu   rsrc1, rsrc2, label
(define bltz    'bltz)   ; bltz   rs, label
(define bltzal  'bltzal) ; bltzal rs, label
(define bne     'bne)    ; bne    rs, rt, label
(define bnez    'bnez)   ; bnez   rsrc, label


(define clo     'clo) ; clo rd, rs
(define clz     'clz) ; clz rd, rs

(define div     'div)  ; div  rs, rt / div  rdest, rsrc1, src2
(define divu    'divu) ; divu rs, rt / divu rdest, rsrc1, src2

(define j       'j)    ; j    target
(define jal     'jal)  ; jal  target
(define jalr    'jalr) ; jalr rs, rd
(define jr      'jr)   ; jr   rs

(define li      'li)  ; li  rdest, imm
(define lui     'lui) ; lui rt, imm

(define la      'la)   ; la   rdest, address
(define lb      'lb)   ; lb   rt, address
(define lbu     'lbu)  ; lbu  rt, address
(define ld      'ld)   ; ld   rdest, address
(define lh      'lh)   ; lh   rt, address
(define lhu     'lhu)  ; lhu  rt, address
(define ll      'll)   ; ll   rt, address
(define lw      'lw)   ; lw   rt, address
(define lwc1    'lwc1) ; lwc1 ft, address
(define lwl     'lwl)  ; lwl  rt, address
(define lwr     'lwr)  ; lwr  rt, address
(define ulh     'ulh)  ; ulh  rdest, address
(define ulhu    'ulhu) ; ulhu rdest, address
(define ulw     'ulw)  ; ulw  rdest, address

(define move    'move) ; move rdest rsrc
(define movf    'movf) ; movf rd, rs, cc
(define movn    'movn) ; movn rd, rs, rt
(define movt    'movt) ; movt rd, rs, cc
(define movz    'movz) ; movz rd, rs, rt
(define mfc0    'mfc0) ; mfc0 rt, rd
(define mfc1    'mfc1) ; mfc1 rt, fs
(define mfhi    'mfhi) ; mfhi rd
(define mflo    'mflo) ; mflo rd
(define mthi    'mthi) ; mthi rs
(define mtlo    'mtlo) ; mtlo rs
(define mtc0    'mtc0) ; mtc0 rd, rt
(define mtc1    'mtc1) ; mtc1 rd, fs

(define madd    'madd)  ; madd  rs, rt
(define maddu   'maddu) ; maddu rs, rt

(define msub    'msub)  ; msub  rs, rt
(define msubu   'msubu) ; msubu rs, rt

(define mul     'mul)   ; mul   rd, rs, rt
(define mulo    'mulo)  ; mulo  rdest, rsrc1, src2
(define mulou   'mulou) ; mulou rdest, rsrc1, src2

(define mult    'mult)  ; mult  rs, rt
(define multu   'multu) ; multu rs, rt

(define neg     'neg)  ; neg  rdest, rsrc
(define negu    'negu) ; negu rdest, rsrc

(define nop     'nop) ; nop

(define nor     'nor) ; nor rd, rs, rt

(define not     'not) ; not rdest, rsrc

(define op-or   'or)  ; or  rd, rs, rt
(define ori     'ori) ; ori rt, rs, imm

(define rem     'rem)  ; rem rdest, rsrc1, rsrc2
(define remu    'remu) ; rem rdest, rsrc1, rsrc2

(define rol     'rol)  ; rol rdest, rsrc1, rsrc2
(define ror     'ror)  ; ror rdest, rsrc1, rsrc2

(define sb      'sb)   ; sb   rt, address
(define sc      'sc)   ; sc   rt, address
(define sd      'sd)   ; sd   rsrc, address
(define sh      'sh)   ; sh   rt, address
(define sw      'sw)   ; sw   rt, address
(define swc1    'swc1) ; swc1 ft, address
(define sdc1    'sdc1) ; sdc1 ft, address
(define swl     'swl)  ; swl  rt, address
(define swr     'swr)  ; swr  rt, address
(define ush     'ush)  ; ush  rsrc, address
(define usw     'usw)  ; usw  rsrc, address

(define seq     'seq)   ; seq   rdest, rsrc1, rsrc2
(define sge     'sge)   ; sge   rdest, rsrc1, rsrc2
(define sgeu    'sgeu)  ; sgeu  rdest, rsrc1, rsrc2
(define sgt     'sgt)   ; sgt   rdest, rsrc1, rsrc2
(define sgtu    'sgtu)  ; sgtu  rdest, rsrc1, rsrc2
(define sle     'sle)   ; sle   rdest, rsrc1, rsrc2
(define sleu    'sleu)  ; sleu  rdest, rsrc1, rsrc2
(define slt     'slt)   ; slt   rd, rs, rt
(define slti    'slti)  ; sltu  rt, rs, imm
(define sltiu   'sltiu) ; sltiu rt, rs, imm
(define sltu    'sltu)  ; sltu  rd, rs, rt
(define sne     'sne)   ; sne   rdest, rsrc1, rsrc2

(define sll     'sll)  ; sll  rd, rt, shamt
(define sllv    'sllv) ; sllv rd, rt, rs
(define sra     'sra)  ; sra  rd, rt, shamt
(define srav    'srav) ; srav rd, rt, rs
(define srl     'sra)  ; srl  rd, rt, shamt
(define srlv    'srav) ; srlv rd, rt, rs

(define sub     'sub)  ; sub  rd, rs, rt
(define subu    'subu) ; subu rd, rs, rt

(define syscall 'syscall) ; syscall

(define xor     'xor)  ; xor  rd, rs, rt
(define xori    'xori) ; xori rt, rs, imm

;; directives
(define .align  '.align)
(define .ascii  '.ascii)
(define .asciiz '.asciiz)
(define .byte   '.byte)
(define .data   '.data)
(define .double '.double)
(define .extern '.extern)
(define .float  '.float)
(define .globl  '.globl)
(define .half   '.half)
(define .kdata  '.kdata)
(define .ktext  '.ktext)
(define .set    '.set)
(define .space  '.space)
(define .text   '.text)
(define .word   '.word)
