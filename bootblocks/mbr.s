!
! This is a 'Master Boot Record' following the MSDOS 'standards'.
! This BB successfully boots MSDOS or Linux.
!
! In addition it has the facility to load and execute a small program
! (of 8 extents) before the boot blocks are checked.
!
! Or
!
! Space for 12 extra partitions in a form that Linux _does_ understand.
!

! Lowest available is $0500, MSDOS appears to use $0600 ... I wonder why?
ORGADDR=$0500
preboot=0	! Include the pre-boot loader.
diskman=1	! Disk manager partitions, allows 16 partitions but
		! don't overwrite this with a LILO BB.

! Include standard layout
org ORGADDR
include sysboot.s

public partition_1
public partition_2
public partition_3
public partition_4

 if diskman=0
org ORGADDR+$3
.ascii "ELKS MBR     Copyright 1996, Robert de Bath"

! Start after dos fsstat data, not strictly required.
org codestart
 else
org ORGADDR
 endif
  cli			! Assume _nothing_!
  cld
  mov	bx,#$7C00	! Pointer to start of BB.
  xor	ax,ax		! Segs all to zero
  mov	ds,ax
  mov	es,ax
  mov	ss,ax
  mov	sp,bx		! SP Just below BB
  mov	cx,#$100	! Move 256 words
  mov	si,bx		! From default BB
  mov	di,#ORGADDR	! To the correct address.
  rep
   movsw
  jmpi	cont,#0		! Set CS:IP correct.
cont:
  sti			! Let the interrupts back in.

!
! Next check for a pre-boot load.

 if preboot
  push	bx
  mov	si,#pre_boot_table
  lodsw
  mov	di,ax		! First word is execute address
more_boot:
  lodsw
  test	ax,ax
  jz	load_done
  mov	bx,ax		! word 1 address
  lodsw
  mov	cx,ax		! word 2 CX, head/sector
  lodsw
  mov	dx,ax		! word 3 DX, drive, cylinder
  lodsw			! word 4 AX, $02, sector count
  int	$13
  jnc	more_boot	! This doesn't retry, with a HD it shouldn't be bad.
  jc	disk_error
load_done:
  call	di
exec_done:
  pop	bx
 endif

! Now check the partition table, must use SI as pointer cause that's what the
! partition boot blocks expect.

  mov	si,#partition_1
check_active:
  cmp	byte [si],#$80			! Flag for activated partition
  jz	found_active
bad_boot:
  add	si,#partition_2-partition_1
  cmp	si,#bootblock_magic
  jnz	check_active

  # Check for Disk manager partitions (12 more!)
 if diskman
  cmp	word ptr diskman_magic,#$55AA
  jnz	no_diskman
  mov	si,#partition_1
check_next:
  sub	si,#partition_2-partition_1
  cmp	byte [si],#$80			! Flag for activated partition
  jz	found_active
  cmp	si,#low_partition
  jnz	check_next

no_diskman:
 endif

  mov	si,#no_bootpart		! Message & boot
  jmp	no_boot

found_active:
  mov	di,#6		! Max retries, int list says 3 ... double it
  mov	dx,[si]		! dh = Drive head, dl = $80 ie HD drive 0
  mov	cx,[si+2]	! cx = Sector & head encoded for int $13
  ! bx is correct
retry:
  movb	[$7DFE],#0	! Clear magic for dosemu
  mov	ax,#$0201	! Read 1 sector
  int   $13		! Disk read.
  jnc	sector_loaded

! Error, reset and retry
  xor	ax,ax
  int	$13		! Disk reset

  dec	di
  jnz	retry		! Try again

disk_error:
  mov	si,#disk_read_error
  jmp	no_boot			! Sorry it ain't gonna work.

sector_loaded:
  mov	di,#$7DFE	! End of sector loaded
  cmp	[di],#$AA55	! Check for magic
  jnz	bad_boot	! No? Try next partition.

  mov	bp,si		! LILO says some BBs use bp rather than si
  jmpi	#$7C00,#0	! Go!

! Fatal errors ...........
no_boot:		! SI now has pointer to error message
  lodsb
  cmp	al,#0
  jz	EOS
  mov	bx,#7
  mov	ah,#$E		! Can't use $13 cause that's AT+ only!
  int	$10
  jmp	no_boot
EOS:
  cmp	si,#press_end	! After msg output 'press key' message
  jz	keyboot
  mov	si,#press_key
  jmp	no_boot

keyboot:		! Wait for a key then reboot
  xor	ax,ax
  int	$16
  int	$19		! This should be OK as we haven't touched anything.
  jmpi	$0,$FFFF	! Wam! Try or die!

no_bootpart:
  .asciz	"No bootable partition"
disk_read_error:
  .asciz	"Error loading system"
press_key:
  .asciz	"\r\nPress a key to reboot"
press_end:

 if preboot
return:
  ret

export pre_boot_table
pre_boot_table:
  ! Example: Load rest of H0,C0 into mem at $7C00 (8k).
  ! .word $7C00, $7C00,$0002,$0000,$0210, $0000
  .word return
  .word	0
 endif

 if diskman

  org ORGADDR+0xFC
public diskman_magic
diskman_magic:
  .word 0xAA55
low_partition:
public partition_16
partition_16 = low_partition+0x00
public partition_15
partition_15 = low_partition+0x10
public partition_14
partition_14 = low_partition+0x20
public partition_13
partition_13 = low_partition+0x30
public partition_12
partition_12 = low_partition+0x40
public partition_11
partition_11 = low_partition+0x50
public partition_10
partition_10 = low_partition+0x60
public partition_9
partition_9 = low_partition+0x70
public partition_8
partition_8 = low_partition+0x80
public partition_7
partition_7 = low_partition+0x90
public partition_6
partition_6 = low_partition+0xA0
public partition_5
partition_5 = low_partition+0xB0
 endif

! Now make sure this isn't to big!
  if *>partition_1
   fail! Partition overlap
  endif

!THE END
