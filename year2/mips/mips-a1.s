	.data
	.align 2
input:	.asciiz "\nEnter number:"
output1:	.asciiz "\nNOT FOUND"
output2:	.asciiz "\nFOUND"
cr:	.asciiz "\n"
	.text
	.globl main

# Program doesn't work correctly, but I tried my best to do the assignment.
# I couldn't find the error in the binary search implementation
main:	
# while loop to add inputs

li 	$s1,-2		        ; # list counter, $s1 - (-1) to count for 0 not being in the list

inputnum:
# Prompt input
li 	$v0,4               ; # System call code 4 for print string
la 	$a0,input           ; # Argument string as input
syscall			        ; # Print the string

# Read input
li $v0,5                ; # System call code 5 to read int input
syscall                 ; # Read it
move $s0,$v0            ; # Move number into s0
addi $s1,$s1,1          ; # Add 1 to list counter 

# Check if input doesn't equal 0
bne  $s0, $zero, else   ; #
# initialise variables
li $t0,0                ; #
move $t1,$s1            ; #
li $t3,5                ; #
j inputnum2             ; #

else:  
addi $sp, $sp, -4       ; # Decrement stack pointer by 4
sw   $s0, 0($sp)        ; # Save $s0 to stack
j inputnum              ; # Loop


inputnum2:
# Prompt input to search
li 	$v0,4               ; # System call code 4 for print string
la 	$a0,input           ; # Argument string as input
syscall			        ; # Print the string

# Read input
li $v0,5                ; # System call code 5 to read int input
syscall                 ; # Read it
move $s0,$v0            ; # Move number into s0

# Check if input doesn't equal 0
bne  $s0, $zero, else5   ; #
j exit                   ; #

else5:  
j binarysearch           ; # Loop

binarysearch:
# $t0 - low
# $t1 - high
# $t2 - mid
# $t3 - value
# $t4 - high-low
# $t5 - mid value address
# $t6 - mid value
# $t7 - value-mid value
# $t8 - temp

# Check if greater or equal to zero
sub $t4,$t1,$t0         ; #
bgez $t4,else2          ; #

# Print not found
li 	$v0,4               ; # System call code 4 for print string
la 	$a0,output1         ; # Argument string as input
syscall			        ; # Print the string
j inputnum2             ; #

else2:

# Calculate mid point
li $t8,2                ; #
sub $t2,$t1,$t0         ; #
div $t2,$t8             ; #
mflo $t2                ; #
add $t2,$t2,$t0

# Get mid value address and mid value
li $t8,4                ; #
sub $t5,$t1,$t2         ; #
mult $t5,$t8            ; #
mflo $t5                ; #
add $t5,$t5,$sp         ; #
lw $t6, 0($t5)          ; #

# Check if less than equal to zero
sub $t7,$t3,$t6         ; #
blez $t7,else3          ; #

# Update variables
move $t1,$t2
#lw $t1,0($t2)          ; #
addi $t1,$t1,-1         ; #
j binarysearch

else3:
# Check if equal zero
beq $t7,$zero,else4     ; #

# Update variables
move $t0,$t2
#lw $t0,0($t2)          ; #
addi $t0,$t0,1          ; #
j binarysearch          ; #

else4:
# Print found
li 	$v0,4               ; # System call code 4 for print string
la 	$a0,output2         ; # Argument string as input
syscall			        ; # Print the string
j inputnum2
        
exit:
li 	$v0,10              ; # System call code for exit
syscall                 ; # exit        