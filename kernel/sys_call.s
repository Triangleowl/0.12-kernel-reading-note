/*
 *  linux/kernel/system_call.s
 *
 *  (C) 1991  Linus Torvalds
 */

/*
 *  system_call.s  contains the system-call low-level handling routines.
 * This also contains the timer-interrupt handler, as some of the code is
 * the same. The hd- and flopppy-interrupts are also here.
 *
 * NOTE: This code handles signal-recognition, which happens every time
 * after a timer-interrupt and after each system call. Ordinary interrupts
 * don't handle signal-recognition, as that would clutter them up totally
 * unnecessarily.
 *
 * Stack layout in 'ret_from_system_call':
 *
 *	 0(%esp) - %eax
 *	 4(%esp) - %ebx
 *	 8(%esp) - %ecx
 *	 C(%esp) - %edx
 *	10(%esp) - original %eax	(-1 if not system call)
 *	14(%esp) - %fs
 *	18(%esp) - %es
 *	1C(%esp) - %ds
 *	20(%esp) - %eip
 *	24(%esp) - %cs
 *	28(%esp) - %eflags
 *	2C(%esp) - %oldesp
 *	30(%esp) - %oldss
 */

SIG_CHLD	= 17

EAX		= 0x00
EBX		= 0x04
ECX		= 0x08
EDX		= 0x0C
ORIG_EAX	= 0x10
FS		= 0x14
ES		= 0x18
DS		= 0x1C
EIP		= 0x20
CS		= 0x24
EFLAGS		= 0x28
OLDESP		= 0x2C
OLDSS		= 0x30

state	= 0		# these are offsets into the task-struct.
counter	= 4
priority = 8
signal	= 12
sigaction = 16		# MUST be 16 (=len of sigaction)
blocked = (33*16)

# offsets within sigaction
sa_handler = 0
sa_mask = 4
sa_flags = 8
sa_restorer = 12

nr_system_calls = 82

ENOSYS = 38

/*
 * Ok, I get parallel printer interrupts while using the floppy for some
 * strange reason. Urgel. Now I just ignore them.
 */
.globl _system_call,_sys_fork,_timer_interrupt,_sys_execve
.globl _hd_interrupt,_floppy_interrupt,_parallel_interrupt
.globl _device_not_available, _coprocessor_error

.align 2
bad_sys_call:
	pushl $-ENOSYS
	jmp ret_from_sys_call
.align 2
reschedule:
	pushl $ret_from_sys_call
	jmp _schedule
.align 2
/*
	system_call 的入口点，由int 0x80触发此中断
*/
_system_call:
	push %ds
	push %es
	push %fs
	pushl %eax		# save the orig_eax
	pushl %edx		# 0.12内核提供的系统调用最多支持传入3个参数
	pushl %ecx		# push %ebx,%ecx,%edx as parameters
	pushl %ebx		# to the system call
	movl $0x10,%edx		# set up ds,es to kernel space	0x10为内核的数据段的段选择子(见head.s的倒数第三行)
	mov %dx,%ds
	mov %dx,%es
	movl $0x17,%edx		# fs points to local data space
	mov %dx,%fs
	cmpl _NR_syscalls,%eax	# NR_syscalls = sizeof(sys_call_table)/sizeof(fn_ptr); 系统调用数，见sys.h，检查系统调用号是否有效
	jae bad_sys_call
	call _sys_call_table(,%eax,4)	# sys_call_table 在 sys.h 中
	pushl %eax				# 系统调用的返回值压栈
2:
	movl _current,%eax		# 获取当前任务（进程）数据结构指针
	cmpl $0,state(%eax)		# state，0 表示就绪态
	jne reschedule			# 如果不是就绪态
	cmpl $0,counter(%eax)	# counter，剩余的时间片
	je reschedule			# 如果counter为0表明时间片已用完，重新调度

/*
	中断服务函数执行完毕，恢复现场，不仅被系统调用这一中断调用，其他中断也会复用ret_from_sys_call
*/
ret_from_sys_call:
	movl _current,%eax
	cmpl _task,%eax			# task[0] cannot have signals
	je 3f					# 如果当前任务为 task[0]则跳转到label 3
	cmpw $0x0f,CS(%esp)		# was old code segment supervisor ? 判断是否是用户态代码段(0x0f: 0b1111, DPL=3), 如果段选择子不是0x0f，
	jne 3f					# 说明不是通过_system_call执行到此处的，那就是从其他的中断jmp到ret_from_sys_call的，所以跳转到label 3
	cmpw $0x17,OLDSS(%esp)	# was stack segment = 0x17 ? 如果远堆栈段的段选择子不是0x17，说明不是从用户态过来的，跳转到label 3
	jne 3f
	movl signal(%eax),%ebx	# 取信号位图 -> ebx，1bit表示1种信号，共32种
	movl blocked(%eax),%ecx	# 取阻塞（屏蔽）信号位图 -> ecx
	notl %ecx				# 按位取反
	andl %ebx,%ecx			# 获得许可的信号位图
	bsfl %ecx,%ecx			# 从低位（位 0）开始扫描位图，看是否有 1 的位， 若有，则 ecx 保留该位的偏移值（即第几位 0--31）
	je 3f					# 如果没有信号则向前跳转退出。
	btrl %ecx,%ebx			# 复位该信号（ebx 含有原 signal 位图）
	movl %ebx,signal(%eax)	# 重新保存 signal 位图信息 current->signal。
	incl %ecx				# 信号值取值范围为[1,32]，故需要对ecx做加一调整
	pushl %ecx				# 信号值作为参数压入栈中
	call _do_signal			# 调用信号处理函数
	popl %ecx				# 弹出信号值
	testl %eax, %eax		
	jne 2b		# see if we need to switch tasks, or do more signals
3:	popl %eax
	popl %ebx
	popl %ecx
	popl %edx
	addl $4, %esp	# skip orig_eax
	pop %fs
	pop %es
	pop %ds
	iret

.align 2
_coprocessor_error:
	push %ds
	push %es
	push %fs
	pushl $-1		# fill in -1 for orig_eax
	pushl %edx
	pushl %ecx
	pushl %ebx
	pushl %eax
	movl $0x10,%eax
	mov %ax,%ds
	mov %ax,%es
	movl $0x17,%eax
	mov %ax,%fs
	pushl $ret_from_sys_call
	jmp _math_error

.align 2
_device_not_available:
	push %ds
	push %es
	push %fs
	pushl $-1		# fill in -1 for orig_eax
	pushl %edx
	pushl %ecx
	pushl %ebx
	pushl %eax
	movl $0x10,%eax
	mov %ax,%ds
	mov %ax,%es
	movl $0x17,%eax
	mov %ax,%fs
	pushl $ret_from_sys_call
	clts				# clear TS so that we can use math
	movl %cr0,%eax
	testl $0x4,%eax			# EM (math emulation bit)
	je _math_state_restore
	pushl %ebp
	pushl %esi
	pushl %edi
	pushl $0		# temporary storage for ORIG_EIP
	call _math_emulate
	addl $4,%esp
	popl %edi
	popl %esi
	popl %ebp
	ret

.align 2
_timer_interrupt:
	push %ds		# save ds,es and put kernel data space
	push %es		# into them. %fs is used by _system_call
	push %fs
	pushl $-1		# fill in -1 for orig_eax
	pushl %edx		# we save %eax,%ecx,%edx as gcc doesn't
	pushl %ecx		# save those across function calls. %ebx
	pushl %ebx		# is saved as we use that in ret_sys_call
	pushl %eax
	movl $0x10,%eax
	mov %ax,%ds
	mov %ax,%es
	movl $0x17,%eax
	mov %ax,%fs
	incl _jiffies
	movb $0x20,%al		# EOI to interrupt controller #1
	outb %al,$0x20
	movl CS(%esp),%eax
	andl $3,%eax		# %eax is CPL (0 or 3, 0=supervisor)
	pushl %eax
	call _do_timer		# 'do_timer(long CPL)' does everything from
	addl $4,%esp		# task switching to accounting ...
	jmp ret_from_sys_call

.align 2
_sys_execve:
	lea EIP(%esp),%eax
	pushl %eax
	call _do_execve
	addl $4,%esp
	ret

.align 2
_sys_fork:
	call _find_empty_process	// 从task结构体中找一个未使用的项，返回其索引
	testl %eax,%eax				// testl 对 src 和 dst 进行与操作，结果不影响src/dst，只影响标志位
	js 1f						// 如果SF置位，即没有找到empty process
	push %gs
	pushl %esi
	pushl %edi
	pushl %ebp
	pushl %eax
	call _copy_process			// 调用 copy_process函数，在fork.c中
	addl $20,%esp				// 将压入栈中的东西丢弃
1:	ret

_hd_interrupt:
	pushl %eax
	pushl %ecx
	pushl %edx
	push %ds
	push %es
	push %fs
	movl $0x10,%eax
	mov %ax,%ds
	mov %ax,%es
	movl $0x17,%eax
	mov %ax,%fs
	movb $0x20,%al
	outb %al,$0xA0		# EOI to interrupt controller #1
	jmp 1f			# give port chance to breathe
1:	jmp 1f
1:	xorl %edx,%edx
	movl %edx,_hd_timeout
	xchgl _do_hd,%edx
	testl %edx,%edx
	jne 1f
	movl $_unexpected_hd_interrupt,%edx
1:	outb %al,$0x20
	call *%edx		# "interesting" way of handling intr.
	pop %fs
	pop %es
	pop %ds
	popl %edx
	popl %ecx
	popl %eax
	iret

_floppy_interrupt:
	pushl %eax
	pushl %ecx
	pushl %edx
	push %ds
	push %es
	push %fs
	movl $0x10,%eax
	mov %ax,%ds
	mov %ax,%es
	movl $0x17,%eax
	mov %ax,%fs
	movb $0x20,%al
	outb %al,$0x20		# EOI to interrupt controller #1
	xorl %eax,%eax
	xchgl _do_floppy,%eax
	testl %eax,%eax
	jne 1f
	movl $_unexpected_floppy_interrupt,%eax
1:	call *%eax		# "interesting" way of handling intr.
	pop %fs
	pop %es
	pop %ds
	popl %edx
	popl %ecx
	popl %eax
	iret

_parallel_interrupt:
	pushl %eax
	movb $0x20,%al
	outb %al,$0x20
	popl %eax
	iret
