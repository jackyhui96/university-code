	.data
	.align 2
input1:	.asciiz "\nEnter number of entries:"
input2:	.asciiz "\nEnter number:"
cr:	.asciiz "\n"
	.text
	.globl main
main:	
# Prompt for number of inputs	
	li 	$v0,4		; # System call code 4 for print string
	la 	$a0,input1	; # Argument string as input
	syscall			; # Print the string
# Read number of inputs
	li 	$v0,5		; # System call code 5 to read int input
	syscall			; # Read it
	move	$s0,$v0		; # Move N into s0
# Allocate space on the heap for N integers
	li	$v0,9		; # code for heap allocation
	li	$t0,4		; # amount of space needed is 4N bytes
	mul	$a0,$s0,$t0	;
	syscall			;
	move	$s1,$v0		; # Base address of array in s1
# Read in the numbers
	li	$s2,0		; # s2 <- Loop counter i = 0
LOOP1:	sub	$t0,$s2,$s0	; # t0 <= i-N
	bgez	$t0,ELOOP1	; # quit loop if i-N>=0
	li 	$v0,4		; # System call code 4 for print string
	la 	$a0,input2	; # Argument string as input
	syscall			; # Print the string
	li	$v0,5		; # System call code 5 to read int input
	syscall			; # Read it
	li	$t0,4		; # Calculate address offset
	mul 	$t0,$s2,$t0	; 
	add 	$t0,$t0,$s1	; # Calculate absolute address
	sw	$v0,($t0)	; # Store number on heap
	addi	$s2,$s2,1	; # increment loop counter
	j	LOOP1		; # Loop
ELOOP1:
	move 	$a0,$s0		; # Copy N into a0 for passing
	move	$a1,$s1		; # Copy address into a1 for passing
	jal 	bubblesort	; # all the subroutine
# Print the sorted array
	li	$s2,0		; # s2 <- Loop counter i = 0
LOOP2:	sub	$t0,$s2,$s0	; # t0 <= i-N
	bgez	$t0,ELOOP2	; # quit loop if i-N>=0
	mul 	$t0,$s2,4	; # Calculate address offset
	add 	$t0,$t0,$s1	; # Calculate absolute address
	lw	$a0,($t0)	; # Load number from heap
	li 	$v0,1		; # System call code 1 for print int
	syscall			; # Print the string
	li	$v0,4		; # System call code 4 to print str
	la	$a0,cr		; # load cr
	syscall			; # Read it
	addi	$s2,$s2,1	; # increment loop counter
	j	LOOP2		; # Loop
ELOOP2:	nop
# Controlled exit
	li 	$v0,10		; # System call code for exit
	syscall

bubblesort:
	# a0 holds N, a1 holds the base address of the array
	# Push things onto the stack
	# t0 holds flag
	# t1 is loop counter
	# t2 is address of current array entry A[i]
	# t3 is A[i]
	# t4 is A[i+1]
	# t5 is used for temp values
# Sort the array
START:	li	$t0,0		; # flag to keep track of swaps initially false
# Sort Outer loop
	li 	$t1,0		; # outer loop variable	
OLOOP:	addi	$t5,$a0,-2	; # t0 <- N-2
	sub 	$t5,$t1,$t5	; # t0 <- i-(N-2)
	bgtz	$t5,EOLOOP	; # Loop termination
	li	$t2,4		; # s2 <- const 4 (bytes/word)
	mul 	$t2,$t1,$t2	; # s2 <- address offset of A[i]
	add 	$t2,$t2,$a1	; # s2 <- absolute address of A[i]
	lw 	$t3,($t2)	; # Load A[i]
	lw 	$t4,4($t2)	; # Load A[i+1]
	sub 	$t5,$t3,$t4	; # t0 <- A[i]-A[i+1]
	blez	$t5,NOSWAP	; # already in order if A[i]-A[i+1] <= 0
	sw	$t3,4($t2)	; # put back in memory in opposite order
	sw	$t4,0($t2)	; # put back in memory in opposite order
	li	$t0,1		; # raise flag to say we've done something
NOSWAP:	nop
	addi	$t1,$t1,1	; # increment outer loop var
	j	OLOOP		; # Loop
EOLOOP:	nop			;
	bne	$t0,$zero,START	; # back to start if sorted flag set
	jr $ra			; # Return
