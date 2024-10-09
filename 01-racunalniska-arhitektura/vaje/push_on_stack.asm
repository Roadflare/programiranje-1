MOV A, 13
MOV B, 1
MOV C, A


pusher:
PUSH C
ADD A, B
MOV C, A
CMP C, 42
JBE pusher

HLT