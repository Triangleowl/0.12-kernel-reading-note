!
! SYS_SIZE is the number of clicks (16 bytes) to be loaded.
! 0x3000 is 0x30000 bytes = 196kB, more than enough for current
! versions of linux
!
#include <linux/config.h>
SYSSIZE = DEF_SYSSIZE
!
!	bootsect.s		(C) 1991 Linus Torvalds
!	modified by Drew Eckhardt
!
! bootsect.s is loaded at 0x7c00 by the bios-startup routines, and moves
! iself out of the way to address 0x90000, and jumps there.
!
! It then loads 'setup' directly after itself (0x90200), and the system
! at 0x10000, using BIOS interrupts. 
!
! NOTE! currently system is at most 8*65536 bytes long. This should be no
! problem, even in the future. I want to keep it simple. This 512 kB
! kernel size should be enough, especially as this doesn't contain the
! buffer cache as in minix
!
! The loader has been made as simple as possible, and continuos
! read errors will result in a unbreakable loop. Reboot by hand. It
! loads pretty fast by getting whole sectors at a time whenever possible.

.globl begtext, begdata, begbss, endtext, enddata, endbss
.text
begtext:
.data
begdata:
.bss
begbss:
.text

SETUPLEN = 4				! nr of setup-sectors
BOOTSEG  = 0x07c0			! original address of boot-sector
INITSEG  = DEF_INITSEG			! we move boot here - out of the way
								! DEF_INITSEG = 0x9000
SETUPSEG = DEF_SETUPSEG			! setup starts here
SYSSEG   = DEF_SYSSEG			! system loaded at 0x10000 (65536).
ENDSEG   = SYSSEG + SYSSIZE		! where to stop loading

! ROOT_DEV & SWAP_DEV are now written by "build".
ROOT_DEV = 0
SWAP_DEV = 0

entry start
start:
	mov	ax,#BOOTSEG
	mov	ds,ax
	mov	ax,#INITSEG
	mov	es,ax
	mov	cx,#256
	sub	si,si
	sub	di,di
	rep						! 循环次数是cx(cx=256), mov一次拷贝两个字节, 所以只需要512/2=256次
	movw					! 注意这不是intel/AT&T汇编语法, 这是要执行 mov es:di, ds:si
							! 即将0x7c00开始的512字节拷贝到0x90000处.
	jmpi	go,INITSEG		! INITSEG是段地址, go是偏移, jmpi是段间跳转(jump intersegment)

go:	mov	ax,cs				! 此时, 段地址已经是0x9000, 而不再是0x07c0
	mov	dx,#0xfef4	! arbitrary value >>512 - disk parm size

	mov	ds,ax				! ds和es也设置为0x9000
	mov	es,ax
	push	ax

	mov	ss,ax		! put stack at 0x9ff00 - 12.
					! ss也设置为0x9000
	mov	sp,dx		! sp设置为0xfef4
/*
 *	Many BIOS's default disk parameter tables will not 
 *	recognize multi-sector reads beyond the maximum sector number
 *	specified in the default diskette parameter tables - this may
 *	mean 7 sectors in some cases.
 *
 *	Since single sector reads are slow and out of the question,
 *	we must take care of this by creating new parameter tables
 *	(for the first disk) in RAM.  We will set the maximum sector
 *	count to 18 - the most we will encounter on an HD 1.44.  
 *
 *	High doesn't hurt.  Low does.
 *
 *	Segments are as follows: ds=es=ss=cs - INITSEG,
 *		fs = 0, gs = parameter table segment
 */

	! 设置 si 寄存器
	push	#0		! 将立即数0压入stack,然后弹出给fs
	pop	fs
	mov	bx,#0x78		! fs:bx is parameter table address
						! fs:bx 存放的软盘驱动参数地址表
	seg fs				! seg fs 的作用是将下一行汇编指令的段寄存器指定为fs寄存器,(即bx的段寄存器用fs,si寄存器的段寄存器为gs)
						! seg fs 只影响[下一行]汇编的段寄存器.
	lgs	si,(bx)			! gs:si is source
						! 相当于 mov gs:si, fs:bx, 作用是将中断向量表中的第0x1E(0x78 = 0x1E * 4)项取出软盘驱动参数,将其放到si
	! 设置 di 寄存器
	mov	di,dx			! es:di is destination
						! es = 0x9000, di = dx = 0xfef4, 即目的地址为 0x9fef4
	mov	cx,#6			! copy 12 bytes
	cld

	rep
	seg gs
	movw				! 相当于 mov es:di, gs:si, 重复6次

	mov	di,dx			! 重复执行6次movw指令, 此时已经将软盘驱动参数拷贝了一份放在 es:dx 处, 下面要做的就是修改拷贝的内容
						! 并让中断向量表的第 0x1E 指向新的参数
	movb	4(di),*18	! patch sector count
						! 修改拷贝后的参数

	seg fs				! 这四行汇编是将 0x1E 号中断向量指向新的参数地址
	mov	(bx),di			! 低地址存放 offset
	seg fs
	mov	2(bx),es		! 高地址存放 segment

	pop	ax				! 弹出ax, ax = 0x9000, 拷贝给 fs/gs 寄存器
	mov	fs,ax
	mov	gs,ax
	
	xor	ah,ah			! reset FDC 
						! 复位软盘控制器, 让其采用新参数
	xor	dl,dl			! dl = 0, 第一个软驱
	int 	0x13		! BIOS 提供的中断服务

! load the setup-sectors directly after the bootblock.
! Note that 'es' is already set up.

! https://en.wikipedia.org/wiki/INT_13H
! BIOS 0x13 中断服务, 功能号存在 ah 中, 参数存在其他寄存器中
! ah = 2 功能是把磁盘内容读到内存中, 此功能需要的参数如下:
! al: 要读取的扇区数量;
! ch: 磁道(柱面号的低8位);
! cl: 开始扇区位(bit0~bit5), 磁道号高2位(bit6~bit7);
! dh: 磁头号;
! dl: 驱动器号(如果是硬盘则bit7要置位);
! es:bx: 指向内存缓冲区的地址;
! 结果保存在如下寄存器中:
! CF: 如果读磁盘出错, 则CF标志位置位;
! ah: 保存返回码， 如果出错, 可以将此码打印到屏幕上;
! al: 返回实际读取的扇区数量;

load_setup:
	xor	dx, dx			! drive 0, head 0
	mov	cx,#0x0002		! sector 2, track 0
	mov	bx,#0x0200		! address = 512, in INITSEG
	mov	ax,#0x0200+SETUPLEN	! service 2, nr of sectors
							! ah = 2, read sector from drive
	int	0x13			! read it
	jnc	ok_load_setup		! ok - continue

	push	ax			! dump error code
						! 上面的读磁盘操作出错才会执行这段指令, 错误码在ah中
	call	print_nl	! 打印回车换行(可能nl是new linue吧)
	mov	bp, sp
	call	print_hex	! 打印错误码
	pop	ax	
	
	xor	dl, dl			! reset FDC
						! 复位软盘控制器
	xor	ah, ah
	int	0x13
	j	load_setup		! 重新执行load_setup操作

ok_load_setup:

! Get disk drive parameters, specifically nr of sectors/track
! ah = 8 功能是读取驱动的参数, 此功能需要的参数有:
! dl: 驱动的索引, 最高位用于区分软盘/硬盘, 软盘为0, 硬盘位1, 即如果读取硬盘驱动的参数, dl = 0x80
! 结果保存在如下寄存器中:
! CF: 错误时置位;
! ah: 返回码；
! bl: 驱动类型(AT/PS2)
! ch: 最大磁道号的低8位;
! cl: 每磁道最大扇区数(位 0-5)，最大磁道号高 2 位(位 6-7);
! dh: 最大磁头数;
! dl: 驱动器数量;
! es:di 软驱磁盘参数表(es被修改了)
	xor	dl,dl
	mov	ah,#0x08		! AH=8 is get drive parameters
						! 使用 BIOS 的 0x13 中断的 8 号功能(Read Drive Parameters)
	int	0x13
	xor	ch,ch			! ch清零, 此时cx中只剩下了每磁道最大扇区数
	seg cs				! 指定下面一行汇编使用 cs 作为段寄存器, 实际此时 cs = ds = es = fs = gs = 0x9000, 所以此命令完全没必要
	mov	sectors,cx		! 每磁道扇区数存放到sectors变量中
	mov	ax,#INITSEG
	mov	es,ax			! 执行完 int 0x13 改变了 es 寄存器, 这里恢复 es

! Print some inane message
! BIOS 0x10 号中断服务: https://en.wikipedia.org/wiki/INT_10H
! 0x10号中断服务的 0x03 和 0x13 号功能用于在屏幕上输出信息
	mov	ah,#0x03		! read cursor pos
	xor	bh,bh
	int	0x10
	
	mov	cx,#9
	mov	bx,#0x0007		! page 0, attribute 7 (normal)
	mov	bp,#msg1
	mov	ax,#0x1301		! write string, move cursor
	int	0x10

! ok, we've written the message, now
! we want to load the system (at 0x10000)

	mov	ax,#SYSSEG
	mov	es,ax		! segment of 0x010000
	call	read_it			! 将system读取到0x10000
	call	kill_motor		! 关闭驱动马达(这样就能知道驱动器的状态了?)
	call	print_nl		! 光标回车换行

! After that we check which root-device to use. If the device is
! defined (!= 0), nothing is done and the given device is used.
! Otherwise, either /dev/PS0 (2,28) or /dev/at0 (2,8), depending
! on the number of sectors that the BIOS reports currently.
! 检测使用哪个根文件系统设备, 如果已经指定了设备就使用指定的设备;
! 如果没有指定就根据BIOS报告的每磁道扇区数来确定.
	seg cs
	mov	ax,root_dev
	or	ax,ax
	jne	root_defined
	seg cs
	mov	bx,sectors
	mov	ax,#0x0208		! /dev/ps0 - 1.2Mb
	cmp	bx,#15
	je	root_defined
	mov	ax,#0x021c		! /dev/PS0 - 1.44Mb
	cmp	bx,#18
	je	root_defined
undef_root:
	jmp undef_root
root_defined:
	seg cs
	mov	root_dev,ax

! after that (everyting loaded), we jump to
! the setup-routine loaded directly after
! the bootblock:
! ---------------------------- 任务完成, 溜了溜了 ---------------------------------
	jmpi	0,SETUPSEG	! 跳转到 0x9020:0000 执行(源码见setup.s)

! This routine loads the system at address 0x10000, making sure
! no 64kB boundaries are crossed. We try to load it as fast as
! possible, loading whole tracks whenever we can.
!
! in:	es - starting address segment (normally 0x1000)
!
sread:	.word 1+SETUPLEN	! sectors read of current track
head:	.word 0			! current head
track:	.word 0			! current track

read_it:
	mov ax,es
	test ax,#0x0fff
die:	jne die			! es must be at 64kB boundary
	xor bx,bx		! bx is starting address within segment
rp_read:
	mov ax,es
	cmp ax,#ENDSEG		! have we loaded all yet?
	jb ok1_read
	ret
ok1_read:
	seg cs
	mov ax,sectors
	sub ax,sread
	mov cx,ax
	shl cx,#9
	add cx,bx
	jnc ok2_read
	je ok2_read
	xor ax,ax
	sub ax,bx
	shr ax,#9
ok2_read:
	call read_track
	mov cx,ax
	add ax,sread
	seg cs
	cmp ax,sectors
	jne ok3_read
	mov ax,#1
	sub ax,head
	jne ok4_read
	inc track
ok4_read:
	mov head,ax
	xor ax,ax
ok3_read:
	mov sread,ax
	shl cx,#9
	add bx,cx
	jnc rp_read
	mov ax,es
	add ah,#0x10
	mov es,ax
	xor bx,bx
	jmp rp_read

read_track:
	pusha
	pusha	
	mov	ax, #0xe2e 	! loading... message 2e = .
	mov	bx, #7
 	int	0x10
	popa		

	mov dx,track
	mov cx,sread
	inc cx
	mov ch,dl
	mov dx,head
	mov dh,dl
	and dx,#0x0100
	mov ah,#2
	
	push	dx				! save for error dump
	push	cx
	push	bx
	push	ax

	int 0x13
	jc bad_rt
	add	sp, #8   	
	popa
	ret

bad_rt:	push	ax				! save error code
	call	print_all			! ah = error, al = read
	
	
	xor ah,ah
	xor dl,dl
	int 0x13
	

	add	sp, #10
	popa	
	jmp read_track

/*
 *	print_all is for debugging purposes.  
 *	It will print out all of the registers.  The assumption is that this is
 *	called from a routine, with a stack frame like
 *	dx 
 *	cx
 *	bx
 *	ax
 *	error
 *	ret <- sp
 *
*/
 
print_all:
	mov	cx, #5		! error code + 4 registers
	mov	bp, sp	

print_loop:
	push	cx		! save count left
	call	print_nl	! nl for readability
	jae	no_reg		! see if register name is needed
	
	mov	ax, #0xe05 + 0x41 - 1
	sub	al, cl
	int	0x10

	mov	al, #0x58 	! X
	int	0x10

	mov	al, #0x3a 	! :
	int	0x10

no_reg:
	add	bp, #2		! next register
	call	print_hex	! print it
	pop	cx
	loop	print_loop
	ret

print_nl:
	mov	ax, #0xe0d	! CR
	int	0x10
	mov	al, #0xa	! LF
	int 	0x10
	ret

/*
 *	print_hex is for debugging purposes, and prints the word
 *	pointed to by ss:bp in hexadecmial.
*/

print_hex:
	mov	cx, #4		! 4 hex digits
	mov	dx, (bp)	! load word into dx
print_digit:
	rol	dx, #4		! rotate so that lowest 4 bits are used
	mov	ah, #0xe	
	mov	al, dl		! mask off so we have only next nibble
	and	al, #0xf
	add	al, #0x30	! convert to 0 based digit, '0'
	cmp	al, #0x39	! check for overflow
	jbe	good_digit
	add	al, #0x41 - 0x30 - 0xa 	! 'A' - '0' - 0xa

good_digit:
	int	0x10
	loop	print_digit
	ret


/*
 * This procedure turns off the floppy drive motor, so
 * that we enter the kernel in a known state, and
 * don't have to worry about it later.
 */
kill_motor:
	push dx
	mov dx,#0x3f2
	xor al, al
	outb
	pop dx
	ret

sectors:
	.word 0

msg1:
	.byte 13,10
	.ascii "Loading"

.org 506
swap_dev:
	.word SWAP_DEV
root_dev:
	.word ROOT_DEV
boot_flag:
	.word 0xAA55

.text
endtext:
.data
enddata:
.bss
endbss:

