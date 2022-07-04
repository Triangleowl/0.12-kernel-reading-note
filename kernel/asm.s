/*
 *  linux/kernel/asm.s
 *
 *  (C) 1991  Linus Torvalds
 */

/*
 * asm.s contains the low-level code for most hardware faults.
 * page_exception is handled by the mm, so that isn't here. This
 * file also handles (hopefully) fpu-exceptions due to TS-bit, as
 * the fpu must be properly saved/resored. This hasn't been tested.
 */

.globl _divide_error,_debug,_nmi,_int3,_overflow,_bounds,_invalid_op
.globl _double_fault,_coprocessor_segment_overrun
.globl _invalid_TSS,_segment_not_present,_stack_segment
.globl _general_protection,_coprocessor_error,_irq13,_reserved
.globl _alignment_check

/*
	把需要调用的函数地址压入栈中
	_do_divide_error 对应 traps.c 中的 do_divide_error() 函数
*/
_divide_error:
	pushl $_do_divide_error
no_error_code:
	xchgl %eax,(%esp)	# _do_divide_error -> eax
	pushl %ebx
	pushl %ecx
	pushl %edx
	pushl %edi
	pushl %esi
	pushl %ebp
	push %ds
	push %es
	push %fs
	pushl $0		# "error code"
	lea 44(%esp),%edx	# 刚好是 do_divide_error 存放在栈中的位置
	pushl %edx
	movl $0x10,%edx	# 取原栈的ss
	mov %dx,%ds
	mov %dx,%es
	mov %dx,%fs
	call *%eax		# 调用 do_divide_error()
	addl $8,%esp	# 对应前面的 pushl %edx 和 pushl $0，call完之后直接将esp加8，无需popl操作了
	pop %fs
	pop %es
	pop %ds
	popl %ebp
	popl %esi
	popl %edi
	popl %edx
	popl %ecx
	popl %ebx
	popl %eax
	iret		# 注意这里是iret，而不是ret，iret会恢复ss/esp

/*
	int1 -- 调试中断的入口，当EFLAGS的TF标志位置1时引发的中断
*/
_debug:
	pushl $_do_int3		# _do_debug
	jmp no_error_code
/*
	int2 -- 非屏蔽中断调用入口点
*/
_nmi:
	pushl $_do_nmi
	jmp no_error_code
/*
	int3 -- 断点指令引起的中断的入口点
*/
_int3:
	pushl $_do_int3
	jmp no_error_code
/*
	int4 -- 溢出出错处理中断入口点
*/
_overflow:
	pushl $_do_overflow
	jmp no_error_code
/*
	int5 -- 边界检查中断入口点
*/
_bounds:
	pushl $_do_bounds
	jmp no_error_code
/*
	int6 -- 无效操作指令中断入口点
*/
_invalid_op:
	pushl $_do_invalid_op
	jmp no_error_code
/*
	int9 -- 协处理器段超出出错中断入口点
*/
_coprocessor_segment_overrun:
	pushl $_do_coprocessor_segment_overrun
	jmp no_error_code
/*
	int15 -- Intel保留的中断入口点
*/
_reserved:
	pushl $_do_reserved
	jmp no_error_code

_irq13:
	pushl %eax
	xorb %al,%al
	outb %al,$0xF0
	movb $0x20,%al
	outb %al,$0x20
	jmp 1f
1:	jmp 1f
1:	outb %al,$0xA0
	popl %eax
	jmp _coprocessor_error
/*
	int8 -- 双出错故障
*/
_double_fault:
	pushl $_do_double_fault
error_code:
	xchgl %eax,4(%esp)		# error code <-> %eax
	xchgl %ebx,(%esp)		# &function <-> %ebx
	pushl %ecx
	pushl %edx
	pushl %edi
	pushl %esi
	pushl %ebp
	push %ds
	push %es
	push %fs
	pushl %eax			# error code，注意error code不是0
	lea 44(%esp),%eax		# offset
	pushl %eax
	movl $0x10,%eax
	mov %ax,%ds
	mov %ax,%es
	mov %ax,%fs
	call *%ebx
	addl $8,%esp
	pop %fs
	pop %es
	pop %ds
	popl %ebp
	popl %esi
	popl %edi
	popl %edx
	popl %ecx
	popl %ebx
	popl %eax
	iret

_invalid_TSS:
	pushl $_do_invalid_TSS
	jmp error_code

_segment_not_present:
	pushl $_do_segment_not_present
	jmp error_code

_stack_segment:
	pushl $_do_stack_segment
	jmp error_code

_general_protection:
	pushl $_do_general_protection
	jmp error_code

_alignment_check:
	pushl $_do_alignment_check
	jmp error_code

