/*
	move_to_user_mode()将特权级从0切换到3
	调用mvoe_to_user_mode时特权级仍然为0，所以move_to_user_mode
	构造了一个用户态中断时切换到内核态时的stack(正常情况下用户态通过中断进入
	内核态时CPU会自动构造此stack)，然后执行iret就可以从内核态“返回”到用户态 :)
	当中断时需要改变特权级时，CPU会自动构造如下的stack:
	 --------------
	|    orig ss   |
	 --------------
	|   orig esp   |
	 --------------
	| orig eflags  |
	 --------------
	|    orig cs   |
	 --------------
	|   orig eip   |
	 --------------
	|    ... ...   |
	|    ... ...   |
*/
#define move_to_user_mode() \
__asm__ ("movl %%esp,%%eax\n\t" \ // 记录此时的esp的值
	"pushl $0x17\n\t" \		// orig ss, 0x17对应的段选择子的特权级为3
	"pushl %%eax\n\t" \		// orig esp
	"pushfl\n\t" \			// orig eflags
	"pushl $0x0f\n\t" \		// orig cs, 0x0f对应的段选择子的特权级也为3
	"pushl $1f\n\t" \		// orig eip, 这是将下面label 1的地址压到stack中
	"iret\n" \				// 执行这条指令的时候EFLAGS的NT是0，不会发生任务切换，只切换特权级
	"1:\tmovl $0x17,%%eax\n\t" \	// 0x17作为段选择子保存在eax中，CPL=3
	"mov %%ax,%%ds\n\t" \			// 给各个段选择子设置为0x17
	"mov %%ax,%%es\n\t" \
	"mov %%ax,%%fs\n\t" \
	"mov %%ax,%%gs" \
	:::"ax")

#define sti() __asm__ ("sti"::)
#define cli() __asm__ ("cli"::)
#define nop() __asm__ ("nop"::)

#define iret() __asm__ ("iret"::)

#define _set_gate(gate_addr,type,dpl,addr) \
__asm__ ("movw %%dx,%%ax\n\t" \
	"movw %0,%%dx\n\t" \
	"movl %%eax,%1\n\t" \
	"movl %%edx,%2" \
	: \
	: "i" ((short) (0x8000+(dpl<<13)+(type<<8))), \
	"o" (*((char *) (gate_addr))), \
	"o" (*(4+(char *) (gate_addr))), \
	"d" ((char *) (addr)),"a" (0x00080000))

#define set_intr_gate(n,addr) \
	_set_gate(&idt[n],14,0,addr)	// type = 14 = 0b1110 --> interrupt gate

#define set_trap_gate(n,addr) \
	_set_gate(&idt[n],15,0,addr)	// type = 15 = 0b1111 --> trap gate

#define set_system_gate(n,addr) \
	_set_gate(&idt[n],15,3,addr)

#define _set_seg_desc(gate_addr,type,dpl,base,limit) {\
	*(gate_addr) = ((base) & 0xff000000) | \
		(((base) & 0x00ff0000)>>16) | \
		((limit) & 0xf0000) | \
		((dpl)<<13) | \
		(0x00408000) | \
		((type)<<8); \
	*((gate_addr)+1) = (((base) & 0x0000ffff)<<16) | \
		((limit) & 0x0ffff); }

#define _set_tssldt_desc(n,addr,type) \
__asm__ ("movw $104,%1\n\t" \
	"movw %%ax,%2\n\t" \
	"rorl $16,%%eax\n\t" \
	"movb %%al,%3\n\t" \
	"movb $" type ",%4\n\t" \
	"movb $0x00,%5\n\t" \
	"movb %%ah,%6\n\t" \
	"rorl $16,%%eax" \
	::"a" (addr), "m" (*(n)), "m" (*(n+2)), "m" (*(n+4)), \
	 "m" (*(n+5)), "m" (*(n+6)), "m" (*(n+7)) \
	)

#define set_tss_desc(n,addr) _set_tssldt_desc(((char *) (n)),addr,"0x89")
#define set_ldt_desc(n,addr) _set_tssldt_desc(((char *) (n)),addr,"0x82")
