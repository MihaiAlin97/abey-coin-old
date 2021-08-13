data
export prime
align 4
LABELV prime
byte 4 2
export getPrime
code
proc getPrime 0 8
file "in/g_main.c"
line 6
;1:
;2:
;3:int prime = 2;
;4:
;5:
;6:public int getPrime(){
line 7
;7:    printf("%d\n", prime);
ADDRGP4 $2
ARGP4
ADDRGP4 prime
INDIRI4
ARGI4
ADDRGP4 printf
CALLI4
pop
line 8
;8:    return prime;
ADDRGP4 prime
INDIRI4
RETI4
LABELV $1
endproc getPrime 0 8
export nextPrime
proc nextPrime 12 0
line 11
;9:}
;10:
;11:public int nextPrime(){
line 12
;12:    int nextPrime = prime, i, div = 1;
ADDRLP4 4
ADDRGP4 prime
INDIRI4
ASGNI4
ADDRLP4 8
CNSTI4 1
ASGNI4
ADDRGP4 $5
JUMPV
LABELV $4
line 13
;13:    while (1){
line 14
;14:        div = 1;
ADDRLP4 8
CNSTI4 1
ASGNI4
line 15
;15:        nextPrime++;
ADDRLP4 4
ADDRLP4 4
INDIRI4
CNSTI4 1
ADDI4
ASGNI4
line 16
;16:        for (i = 2; i < nextPrime; ++i) {
ADDRLP4 0
CNSTI4 2
ASGNI4
ADDRGP4 $10
JUMPV
LABELV $7
line 17
;17:            if(nextPrime%i == 0) {
ADDRLP4 4
INDIRI4
ADDRLP4 0
INDIRI4
MODI4
CNSTI4 0
NEI4 $11
line 18
;18:                div = 0;
ADDRLP4 8
CNSTI4 0
ASGNI4
line 19
;19:                break;
ADDRGP4 $9
JUMPV
LABELV $11
line 21
;20:            }
;21:        }
LABELV $8
line 16
ADDRLP4 0
ADDRLP4 0
INDIRI4
CNSTI4 1
ADDI4
ASGNI4
LABELV $10
ADDRLP4 0
INDIRI4
ADDRLP4 4
INDIRI4
LTI4 $7
LABELV $9
line 23
;22:
;23:        if (div){
ADDRLP4 8
INDIRI4
CNSTI4 0
EQI4 $13
line 24
;24:            prime = nextPrime;
ADDRGP4 prime
ADDRLP4 4
INDIRI4
ASGNI4
line 25
;25:            break;
ADDRGP4 $6
JUMPV
LABELV $13
line 27
;26:        }
;27:    }
LABELV $5
line 13
ADDRGP4 $4
JUMPV
LABELV $6
line 28
;28:    return 0;
CNSTI4 0
RETI4
LABELV $3
endproc nextPrime 12 0
export instructionCount
proc instructionCount 16 8
line 31
;29:}
;30:
;31:public int instructionCount(){
line 32
;32:	int a=2, b=3, sum=0;
ADDRLP4 0
CNSTI4 2
ASGNI4
ADDRLP4 4
CNSTI4 3
ASGNI4
ADDRLP4 8
CNSTI4 0
ASGNI4
line 33
;33:	sum = a+b;
ADDRLP4 8
ADDRLP4 0
INDIRI4
ADDRLP4 4
INDIRI4
ADDI4
ASGNI4
line 34
;34:	printf("%d\n", getInstructionCount());
ADDRLP4 12
ADDRGP4 getInstructionCount
CALLI4
ASGNI4
ADDRGP4 $2
ARGP4
ADDRLP4 12
INDIRI4
ARGI4
ADDRGP4 printf
CALLI4
pop
line 35
;35:}
CNSTI4 0
RETI4
LABELV $15
endproc instructionCount 16 8
export nextNprimes
proc nextNprimes 16 0
line 37
;36:
;37:public int nextNprimes(int n){
line 38
;38:    int nextPrime = prime, i, div = 1, nextN = 1;
ADDRLP4 4
ADDRGP4 prime
INDIRI4
ASGNI4
ADDRLP4 8
CNSTI4 1
ASGNI4
ADDRLP4 12
CNSTI4 1
ASGNI4
ADDRGP4 $18
JUMPV
LABELV $17
line 39
;39:    while (1){
line 40
;40:        div = 1;
ADDRLP4 8
CNSTI4 1
ASGNI4
line 41
;41:        nextPrime++;
ADDRLP4 4
ADDRLP4 4
INDIRI4
CNSTI4 1
ADDI4
ASGNI4
line 42
;42:        for (i = 2; i < nextPrime; ++i) {
ADDRLP4 0
CNSTI4 2
ASGNI4
ADDRGP4 $23
JUMPV
LABELV $20
line 43
;43:            if(nextPrime%i == 0) {
ADDRLP4 4
INDIRI4
ADDRLP4 0
INDIRI4
MODI4
CNSTI4 0
NEI4 $24
line 44
;44:                div = 0;
ADDRLP4 8
CNSTI4 0
ASGNI4
line 45
;45:                break;
ADDRGP4 $22
JUMPV
LABELV $24
line 47
;46:            }
;47:        }
LABELV $21
line 42
ADDRLP4 0
ADDRLP4 0
INDIRI4
CNSTI4 1
ADDI4
ASGNI4
LABELV $23
ADDRLP4 0
INDIRI4
ADDRLP4 4
INDIRI4
LTI4 $20
LABELV $22
line 49
;48:
;49:        if (div){
ADDRLP4 8
INDIRI4
CNSTI4 0
EQI4 $26
line 50
;50:            nextN++;
ADDRLP4 12
ADDRLP4 12
INDIRI4
CNSTI4 1
ADDI4
ASGNI4
line 51
;51:            if(nextN>n){
ADDRLP4 12
INDIRI4
ADDRFP4 0
INDIRI4
LEI4 $28
line 52
;52:                prime = nextPrime;
ADDRGP4 prime
ADDRLP4 4
INDIRI4
ASGNI4
line 53
;53:                break;
ADDRGP4 $19
JUMPV
LABELV $28
line 56
;54:            }
;55:
;56:        }
LABELV $26
line 57
;57:    }
LABELV $18
line 39
ADDRGP4 $17
JUMPV
LABELV $19
line 58
;58:    return 0;
CNSTI4 0
RETI4
LABELV $16
endproc nextNprimes 16 0
import getInstructionCount
import printf
lit
align 1
LABELV $2
byte 1 37
byte 1 100
byte 1 10
byte 1 0
