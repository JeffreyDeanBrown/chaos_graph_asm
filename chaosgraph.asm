;	Executable name : chaosgraph
;	Created date	: 02/10/2024
;	Last update		: 02/15/2024
;	Author			: Jeffrey Brown
;	Description		: This will approximate Pierre François Verhulst's logistic function
;					  and graph it onto a 379x94 terminal (max size of konsole terminal)
;					  as a 360x80 graph. It uses floating point arithematic and a terminal
;					  graphing style discussed by Jeff Duntemann in their book:
;					    "Assembly Langauge Step by Step: Programming with Linux" 3rd Ed.
;						chapter 11
;                       this was made as a way to practice writing i386 and x87 assembly,
;                       if you want to continue to work on this, the first thing that needs
;                       to happen is a solid reformat and organization
;
;

;
;
;	Built using these commands:
;
;   sandbox: sandbox.o
; 	ld -o sandbox sandbox.o -m elf_i386
; sandbox.o: sandbox.asm
; 	nasm -f elf -g -F dwarf sandbox.asm -l sandbox.lst
;
;
;

; Rough cut:
;
; ; Needs to be moved to appropriate sections and possible macros assigned
; ; simple things such as macros or initialed data should be taken care of
; ; then floating point arithematic needs to be figured out
;
;
; Logistic Equation:
;                   x(n+1) = r*x(n)*[1-x(n)]
;
; ; two Xn values will be stored at a time.
; ; X(n)   in eax, and
; ; X(n+1) in ebx

;
;
;
;
;
;
;
;
;
;                        REMINDER TO SELF: Figure out the weirdness with addressing EQU or dd or resd
;
;
;
;
;
;
;
;
;
;
;


section .data

         r: dd 0.9
      rinc: dd 0.00833
        Xo: dd 0.9
      Rows: dd ROWS
      Zero: dd 0.0

   NUPLOT   EQU 20
   CONVITER EQU 100

;Terminal graphic information:
;           Creating the graphing space to be printed onto a 379x94 terminal (max window size for Konsole)
;               There will be a 360x81 graph, with the left-hand side filled with pipes "|"
;               and the bottom filled with Box characters "─". The entire graph is 360 x 81 = 29160 ASCII characters long
;               There will be a 5 line top margin, 10 line bottom margin, 5 space left margin, and a 14 space right margin
;               top and bottom margins will be 'new line' characters (10), and left and right will be spaces (32)
;               so each line of the  "graph" will be 379 characters long (5 for left margin, 360 for graph, 14 for right margin)
;
;               The entire string is 7 (top margin) + 30699 (379 x 81 = 30699) + 10 (bottom margin) = 30716 bytes long


  LINESIZE    EQU 379
  GRAPHSPACE  EQU 30699
  GRAPHLENGTH EQU 360
  TOPMARGIN   EQU 7
  BTMMARGIN   EQU 7
  LFTMARGIN   EQU 5
  ROWS        EQU 80
  ;Right Margin doesn't need to be calculated




align 16                       ;FPU backups need to be aligned to 16bytes
section .bss

        FPUBackup:      resb 1024   ;place to save FPU

        Yaxis:          resb 1024        ;finished data converted to Yaxis value (stored as a list of 2-byte words)
        Xaxis:          resb 4           ;current position in Xaxis (increments each time r is increased)

        Xn:             resd        1
        Xni:            resd        1

        OutputString:   resb 65536      ;String to graph stored here
        GraphStart:     resd 16         ;Location past the top margin stored here


section .text


;-------------------------------------------------------------------------
; CalculateX: 	Calculates X(0) through X(n) where n = ConvIter
; UPDATED: 	02/10/2024
; IN: 		The memory location of double float r-value in edi
;           The memory lcoation to store Xn             in eax
;           The memory location of double float Xo      in ebx
;           The number of iterations for Xn             in ecx
;
;               OPTIONAL running CalculateX.calc with ecx set to 0
;                         will calculate X(n+1) without modfying FPU registers or ecx
;                               This requires st2 = 1,
;                                             st1 = r,
;                                             st0 = X(n)
;                               loaded into the FPU registers
; RETURNS:	X(n) in memory location eax
;               OPTIONAL call of CalculateX.calc with ecx set to 0 returns:
;                        X(n+1) in st0
; MODIFIES: Nothing
;               OPTIONAL call of CalculateX.calc with ecx set to 0 modifies:
;                        FPU register st0
;                        The FPU register will be loaded with one value,
;                             make sure you have atleast 1 free register
;                             (st1 through st4 will not be changed)
; CALLS:	Nothing
; DESCRIPTION:	Calculates X(n) ecx number of times and returns the final value to address ebx.
;               Values beyond ecx can be calculated by calling CalculateX.calc,
;               but some registers will be modified (see Optional information above)



CalculateX:
;         Calcuate Xn where n is the value in ecx
        finit
        fld1                   ;load 1 onto F7     (st2) (st3 during .calc line 2)
        fld     dword [edi]      ;load r to F6     (st1)
        fld     dword [ebx]     ;load Xo onto F5   (st0) (st1 and st0 during .calc lines 2 and 3)

.calc:  fld      st0      ;duplicate st0 to st0 and st1 (both now equal Xn)
        fsubr    st3      ;st0 = st3-st0 = 1-Xn
        fmulp             ;st0 = st1*st0 = Xn*(1-Xn) (and st1 pops off, making st0 = Xn*(1-Xn))
        fmul     st1      ;st0 = st0*st1 = Xn*(1-Xn)*r = X(n+1)

        cmp   ecx,1
        jb   .ret       ;if .calc was called with ecx = 0, run once
                        ;    without modifying ecx or FPU registers
        sub   ecx,1     ;otherwise, decrement ecx
        je   .home      ;when the last calcuation is done, go home

        jmp  .calc      ;otherwise, repeat for all of ecx (ecx = 100 will run 100 times)

.home:
        fstp  dword  [eax]        ;store the last X(n) value and pop it off the stack
        finit                     ;clear the FPU register stack
.ret:   ret                       ;and head on home






;-----------------------------------------------------------------------------------------------------------------------------------------------------------





DoAGraph:       ;setup the registers for memory calls by the FPU
                mov     eax, Xn
                mov     ebx, Xo
                mov     ecx, CONVITER
                mov     edx, Yaxis
                mov     esi, Rows
                mov     edi, r
                ;calculate Xn where n = CONVITER
GrabSomeData:   call    CalculateX
                ;setup to calculate the next 20 values and scale them to size for the graph
                xor     ecx, ecx    ;for the optional mode of CalculateX.calc
                mov     ebx, NUPLOT ;number of x(n+1) to plot for each r-value
                dec     ebx         ;take a value and use it as an offset
                mov     eax, [Xn]
                mov     [Xni], eax
                mov     eax, Xni
Converge:       finit
                fld1                     ;st2 = 1
                fld     dword [edi]      ;st1 = r
                fld     dword [eax]      ;st0 = Xni
                call    CalculateX.calc  ;st0 = Xn+1, st1 and st2 unaffected  (st1)
                fst     dword [eax]      ;save Xn+1
                fild    dword [esi]      ;load size of Y-axis to FPU          (st0)
                fmulp                    ;multiply int by flt, pop int        (result in st0)
                fistp   word [edx+ebx*2] ;round the value down and store in memory as an interger, pop st0
                sub     ebx, 1
                jnb     Converge
                ret


GraphDots:      mov     ecx, NUPLOT           ;number of values in [edx]
                xor     ebx, ebx
                dec     ecx                   ;use as an offset and a countdown
                mov     edi, [GraphStart]
   .plotdot:    mov     esi, LINESIZE
                mov     eax, ROWS             ;load number of rows
                mov     bx, word [Yaxis+ecx*2];grab a value to graph
                sub     eax, ebx              ;subtract value from 80 (without this the y-axis is reversed, the top is 0 and the bottom is 80)
                dec     eax                   ;use as an offset
                mul     esi                   ;eax now contains which line we are writing a dot to
                add     eax, LFTMARGIN+1      ;offset to X = 0 (without left margin or the space containing the axis "|" itself
                add     eax, edi              ;change this value to an address
                mov     esi, [Xaxis]
                mov     [esi+eax], byte 219    ;write an ASCII "."
                sub     ecx, 1
                jnc    .plotdot
                ret


;-----------------------------------------------------------------------------------------------------------------------------------------------------------
;           Creating the graphing space to be printed onto a 379x94 terminal (max window size for Konsole)
;               There will be a 360x81 graph, with the left-hand side filled with pipes "|"
;               and the bottom filled with Box characters "─". The entire graph is 360 x 81 = 29160 ASCII characters long
;               There will be a 5 line top margin, 10 line bottom margin, 5 space left margin, and a 14 space right margin
;               top and bottom margins will be 'new line' characters (10), and left and right will be spaces (32)
;               so each line of the  "graph" will be 379 characters long (5 for left margin, 360 for graph, 14 for right margin)
;
;               The entire string is 7 (top margin) + 30699 (379 x 81 = 30699) + 10 (bottom margin) = 30716 bytes long

SetupGraph:
                pushad
                ;set the address where the graph string starts
                mov     eax, OutputString
                add     eax, TOPMARGIN
                mov     [GraphStart], eax     ;set the address where the graph starts, i.e. the output + the top margin
                ;write new line characters for the top margin
                mov     eax, 10               ;ASCII new line character
                mov     edi, OutputString
                mov     ecx, TOPMARGIN        ;write new lines for the top margin
                cld                           ;just to be safe
                rep     stosb                 ;write the top margin to the string
                ;fill in the graph space will ASCII space characters
                mov     eax, 32               ;ASCII space
                mov     edi, [GraphStart]
                mov     ecx, GRAPHSPACE       ;for every byte in the graph space,
                rep     stosb                 ;write a space character
                ;fill in the pipe symbol to denote the y-axis
                mov     edi, [GraphStart]
                mov     [edi+LFTMARGIN], byte 124     ;124 is ASCII pipe symbol "|"
                mov     eax, LINESIZE                 ;379 char per line
                mov     ebx, eax                      ;extra copy of linesize for additions
                mov     ecx, 79                       ;first line already has a pipe, last line will get a pipe (81-2=79)
       .Piper:  mov     [edi+eax+LFTMARGIN], byte 124 ;graphstart + size of one line + margin space
                add     eax,ebx                       ;add the value of one line, i.e. go to next line in string
                sub     ecx, 1
                jne     .Piper                         ;repeat for all 79 lines
                ;fill in the x-axis with box character "─"
                add     edi, eax         ;address needs to be in edi
                add     edi, LFTMARGIN   ;after the left margin
                inc     edi              ;one space after the y-axis
                mov     eax, 196         ;Box character "─"
                mov     ecx, GRAPHLENGTH ;the actual graph is 360 long
                rep     stosb
                ;fill in the bottom margin with end of line characters
                mov     edi, [GraphStart]
                add     edi, GRAPHSPACE
                mov     eax, 10         ;new line character
                mov     ecx, BTMMARGIN
                rep     stosb
                popad
                ret


;-------------------------------------------------------------------------------------------------------------------------------------------
        PrintGraph:
                pushad		  ; Save all caller's GP registers
                mov eax,4	  ; Specify sys_write call
                mov ebx,1	  ; Specify File Descriptor 1: Standard output
                mov ecx,OutputString	  ; Pass offset of line string
                mov edx,TOPMARGIN+GRAPHSPACE+BTMMARGIN	  ; Pass size of the line string
                int 80h		  ; Make kernel call to display line string
                popad		  ; Restore all caller's GP registers

                Exit:	mov eax,1		; Code for Exit Syscall
                mov ebx,0		; Return a code of zero
                int 80H			; Make kernel call


global _start

_start:
                nop

                call SetupGraph
                call DoAGraph
                call GraphDots
                mov  ecx, 360
                mov  ebp, 360
 NextR:          finit
                 fld  dword [r]         ;st0
                 fld dword [rinc]     ;result in st0
                 faddp st1
                 fstp dword [r]
                 finit
                 inc  dword [Xaxis]
                 push ecx
                call DoAGraph
                call GraphDots
                 pop  ecx
                 sub  ebp, 1
                 sub  ecx, 1
                 jnb  NextR
                call PrintGraph




















;               OPTIONAL running CalculateX.calc with ecx set to 0
;                         will calculate X(n+1) without modfying FPU registers or ecx
;                               This requires st2 = 1,
;                                             st1 = r,
;                                             st0 = X(n)
;                               loaded into the FPU registers
