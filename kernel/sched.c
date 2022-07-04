/*
 *  linux/kernel/sched.c
 *
 *  (C) 1991  Linus Torvalds
 */

/*
 * 'sched.c' is the main kernel file. It contains scheduling primitives
 * (sleep_on, wakeup, schedule etc) as well as a number of simple system
 * call functions (type getpid(), which just extracts a field from
 * current-task
 */
#include <linux/sched.h>
#include <linux/kernel.h>
#include <linux/sys.h>
#include <linux/fdreg.h>
#include <asm/system.h>
#include <asm/io.h>
#include <asm/segment.h>

#include <signal.h>

#define _S(nr) (1<<((nr)-1))	// 取bit所在的位，如bit5，表示2^(5-1)=16号信号
#define _BLOCKABLE (~(_S(SIGKILL) | _S(SIGSTOP)))	// SIGKILL和SIGSTOP这两个信号不可以阻塞

/*
	内核调试函数，显示任务号为nr的进程的信息
*/
void show_task(int nr,struct task_struct * p)
{
	int i,j = 4096-sizeof(struct task_struct);

	printk("%d: pid=%d, state=%d, father=%d, child=%d, ",nr,p->pid,
		p->state, p->p_pptr->pid, p->p_cptr ? p->p_cptr->pid : -1);
	i=0;
	while (i<j && !((char *)(p+1))[i])
		i++;
	printk("%d/%d chars free in kstack\n\r",i,j);
	printk("   PC=%08X.", *(1019 + (unsigned long *) p));
	if (p->p_ysptr || p->p_osptr) 
		printk("   Younger sib=%d, older sib=%d\n\r", 
			p->p_ysptr ? p->p_ysptr->pid : -1,
			p->p_osptr ? p->p_osptr->pid : -1);
	else
		printk("\n\r");
}

/*
	遍历进程链表，打印所有进程的信息
*/
void show_state(void)
{
	int i;

	printk("\rTask-info:\n\r");
	for (i=0;i<NR_TASKS;i++)
		if (task[i])
			show_task(i,task[i]);
}

/*
	PC 机 8253 计数/定时芯片的输入时钟频率约为 1.193180MHz。Linux 内核希望定时器中断频率是
	100Hz，也即每 10ms 发出一次时钟中断。因此这里 LATCH 是设置 8253 芯片的初值，参见 438 行。
*/
#define LATCH (1193180/HZ)

extern void mem_use(void);

extern int timer_interrupt(void);	// 定时中断程序，kernel/system_call.s
extern int system_call(void);		// 系统调用中断程序, kernel/system_call.s

union task_union {
	struct task_struct task;
	char stack[PAGE_SIZE];
};

static union task_union init_task = {INIT_TASK,};

unsigned long volatile jiffies=0;	// 开机后的滴答数，每10ms加一
unsigned long startup_time=0;		// 开机时间，距1970-01-01的秒数
int jiffies_offset = 0;		/* # clock ticks to add to get "true	// 这个变量用于累计需要调整的时间嘀嗒数
				   time".  Should always be less than
				   1 second's worth.  For time fanatics
				   who like to syncronize their machines
				   to WWV :-) */

struct task_struct *current = &(init_task.task);	// current永远指向当前任务
struct task_struct *last_task_used_math = NULL;

struct task_struct * task[NR_TASKS] = {&(init_task.task), };	// 定义任务数组，第一项指向任务0

long user_stack [ PAGE_SIZE>>2 ] ;	// 定义用户态程序的stack，刚开始内核初始化过程中被用作内核stack，初始化任务完成后被用作任务0的用户态stack

struct {
	long * a;		// esp
	short b;		// ss
	} stack_start = { & user_stack [PAGE_SIZE>>2] , 0x10 };
/*
 *  'math_state_restore()' saves the current math information in the
 * old math state array, and gets the new ones from the current task
 */
void math_state_restore()
{
	if (last_task_used_math == current)
		return;
	__asm__("fwait");
	if (last_task_used_math) {
		__asm__("fnsave %0"::"m" (last_task_used_math->tss.i387));
	}
	last_task_used_math=current;
	if (current->used_math) {
		__asm__("frstor %0"::"m" (current->tss.i387));
	} else {
		__asm__("fninit"::);
		current->used_math=1;
	}
}

/*
 *  'schedule()' is the scheduler function. This is GOOD CODE! There
 * probably won't be any reason to change this, as it should work well
 * in all circumstances (ie gives IO-bound processes good response etc).
 * The one thing you might take a look at is the signal-handler code here.
 *
 *   NOTE!!  Task 0 is the 'idle' task, which gets called when no other
 * tasks can run. It can not be killed, and it cannot sleep. The 'state'
 * information in task[0] is never used.
 */
void schedule(void)
{
	int i,next,c;
	struct task_struct ** p;

/* check alarm, wake up any interruptible tasks that have got a signal */
	for(p = &LAST_TASK ; p > &FIRST_TASK ; --p)	// 从最后一个任务开始，遍历所有的任务(不包括任务0)，检查 alarm
		if (*p) {	// 如果任务不存在，跳过
			if ((*p)->timeout && (*p)->timeout < jiffies) {	// 如果设置了任务超时定时值timeout，且已经超时
				(*p)->timeout = 0;	// 将超时定时值清空
				if ((*p)->state == TASK_INTERRUPTIBLE)	// 如果任务处于可中断睡眠状态
					(*p)->state = TASK_RUNNING;			// 设置为就绪态
			}
			if ((*p)->alarm && (*p)->alarm < jiffies) {	// 如果设置了任务的SIGALRM信号超时定时器，并且已经过期(alarm < jiffies)
				(*p)->signal |= (1<<(SIGALRM-1)); // 在bitmap中设置alarm对应的bit
				(*p)->alarm = 0;
			}
			if (((*p)->signal & ~(_BLOCKABLE & (*p)->blocked)) &&	// 如果信号的bitmap中存在不可阻塞的信号
			(*p)->state==TASK_INTERRUPTIBLE)						// 并且任务状态为可中断睡眠状态
				(*p)->state=TASK_RUNNING;							// 调整为就绪态
		}

/* this is the scheduler proper: */
	/*
		调度程序的主要部分
	*/
	while (1) {
		c = -1;		// 记录最大的时间片
		next = 0;	// 记录最大时间片的任务的索引
		i = NR_TASKS;
		p = &task[NR_TASKS];	// 从最后一个任务开始向前遍历
		while (--i) {	// 遍历所有任务
			if (!*--p)	// 为空跳过
				continue;
			if ((*p)->state == TASK_RUNNING && (*p)->counter > c)	// 如果当前进程状态为TASK_RUNNING,且剩余时间片比c大
				c = (*p)->counter, next = i;	// 记录当前任务的时间片和在task_struct数组中的索引
		}
		if (c) break;	// 如果有进程的时间片不为空，则直接跳出循环，执行switch_to()
		for(p = &LAST_TASK ; p > &FIRST_TASK ; --p)
			if (*p)
				(*p)->counter = ((*p)->counter >> 1) +
						(*p)->priority;
	}
	switch_to(next);	// 在sched.h中定义的宏
}
/*
	pause系统调用，让出CPU
*/
int sys_pause(void)
{
	current->state = TASK_INTERRUPTIBLE;
	schedule();
	return 0;
}
/*
	
*/
static inline void __sleep_on(struct task_struct **p, int state)
{
	struct task_struct *tmp;

	if (!p)
		return;
	if (current == &(init_task.task))	// 任务0不允许sleep :)
		panic("task[0] trying to sleep");
	/*
		将当前任务加入到struct task_struct的头部，p指向一个task_struct链表，
		此链表是一个等待队列，因为是链表，所以加入链表头部最方便:)
	*/
	tmp = *p;			
	*p = current;
	current->state = state;		// 注意这里的state是传入的参数，所以state可能是TASK_INTERRUPTIBLE，也可以是TASK_UNINTERRUPTIBLE
repeat:	schedule();
	if (*p && *p != current) {
		(**p).state = 0;
		current->state = TASK_UNINTERRUPTIBLE;
		goto repeat;
	}
	if (!*p)
		printk("Warning: *P = NULL\n\r");
	if (*p = tmp)
		tmp->state=0;
}

void interruptible_sleep_on(struct task_struct **p)
{
	__sleep_on(p,TASK_INTERRUPTIBLE);
}

void sleep_on(struct task_struct **p)
{
	__sleep_on(p,TASK_UNINTERRUPTIBLE);
}

void wake_up(struct task_struct **p)
{
	if (p && *p) {
		if ((**p).state == TASK_STOPPED)
			printk("wake_up: TASK_STOPPED");
		if ((**p).state == TASK_ZOMBIE)
			printk("wake_up: TASK_ZOMBIE");
		(**p).state=0;
	}
}

/*
 * OK, here are some floppy things that shouldn't be in the kernel
 * proper. They are here because the floppy needs a timer, and this
 * was the easiest way of doing it.
 */
static struct task_struct * wait_motor[4] = {NULL,NULL,NULL,NULL};
static int  mon_timer[4]={0,0,0,0};
static int moff_timer[4]={0,0,0,0};
unsigned char current_DOR = 0x0C;

int ticks_to_floppy_on(unsigned int nr)
{
	extern unsigned char selected;
	unsigned char mask = 0x10 << nr;

	if (nr>3)
		panic("floppy_on: nr>3");
	moff_timer[nr]=10000;		/* 100 s = very big :-) */
	cli();				/* use floppy_off to turn it off */
	mask |= current_DOR;
	if (!selected) {
		mask &= 0xFC;
		mask |= nr;
	}
	if (mask != current_DOR) {
		outb(mask,FD_DOR);
		if ((mask ^ current_DOR) & 0xf0)
			mon_timer[nr] = HZ/2;
		else if (mon_timer[nr] < 2)
			mon_timer[nr] = 2;
		current_DOR = mask;
	}
	sti();
	return mon_timer[nr];
}

void floppy_on(unsigned int nr)
{
	cli();
	while (ticks_to_floppy_on(nr))
		sleep_on(nr+wait_motor);
	sti();
}

void floppy_off(unsigned int nr)
{
	moff_timer[nr]=3*HZ;
}

void do_floppy_timer(void)
{
	int i;
	unsigned char mask = 0x10;

	for (i=0 ; i<4 ; i++,mask <<= 1) {
		if (!(mask & current_DOR))
			continue;
		if (mon_timer[i]) {
			if (!--mon_timer[i])
				wake_up(i+wait_motor);
		} else if (!moff_timer[i]) {
			current_DOR &= ~mask;
			outb(current_DOR,FD_DOR);
		} else
			moff_timer[i]--;
	}
}

#define TIME_REQUESTS 64

static struct timer_list {
	long jiffies;
	void (*fn)();
	struct timer_list * next;
} timer_list[TIME_REQUESTS], * next_timer = NULL;

void add_timer(long jiffies, void (*fn)(void))
{
	struct timer_list * p;

	if (!fn)
		return;
	cli();
	if (jiffies <= 0)
		(fn)();
	else {
		for (p = timer_list ; p < timer_list + TIME_REQUESTS ; p++)
			if (!p->fn)
				break;
		if (p >= timer_list + TIME_REQUESTS)
			panic("No more time requests free");
		p->fn = fn;
		p->jiffies = jiffies;
		p->next = next_timer;
		next_timer = p;
		while (p->next && p->next->jiffies < p->jiffies) {
			p->jiffies -= p->next->jiffies;
			fn = p->fn;
			p->fn = p->next->fn;
			p->next->fn = fn;
			jiffies = p->jiffies;
			p->jiffies = p->next->jiffies;
			p->next->jiffies = jiffies;
			p = p->next;
		}
	}
	sti();
}

void do_timer(long cpl)
{
	static int blanked = 0;

	if (blankcount || !blankinterval) {
		if (blanked)
			unblank_screen();
		if (blankcount)
			blankcount--;
		blanked = 0;
	} else if (!blanked) {
		blank_screen();
		blanked = 1;
	}
	if (hd_timeout)
		if (!--hd_timeout)
			hd_times_out();

	if (beepcount)
		if (!--beepcount)
			sysbeepstop();

	if (cpl)
		current->utime++;
	else
		current->stime++;

	if (next_timer) {
		next_timer->jiffies--;
		while (next_timer && next_timer->jiffies <= 0) {
			void (*fn)(void);
			
			fn = next_timer->fn;
			next_timer->fn = NULL;
			next_timer = next_timer->next;
			(fn)();
		}
	}
	if (current_DOR & 0xf0)
		do_floppy_timer();
	if ((--current->counter)>0) return;
	current->counter=0;
	if (!cpl) return;
	schedule();
}

int sys_alarm(long seconds)
{
	int old = current->alarm;

	if (old)
		old = (old - jiffies) / HZ;
	current->alarm = (seconds>0)?(jiffies+HZ*seconds):0;
	return (old);
}

int sys_getpid(void)
{
	return current->pid;
}

int sys_getppid(void)
{
	return current->p_pptr->pid;
}

int sys_getuid(void)
{
	return current->uid;
}

int sys_geteuid(void)
{
	return current->euid;
}

int sys_getgid(void)
{
	return current->gid;
}

int sys_getegid(void)
{
	return current->egid;
}

int sys_nice(long increment)
{
	if (current->priority-increment>0)
		current->priority -= increment;
	return 0;
}

void sched_init(void)
{
	int i;
	struct desc_struct * p;	// 描述符结构体指针

	if (sizeof(struct sigaction) != 16)
		panic("Struct sigaction MUST be 16 bytes");

	// 在全局描述符中设置任务0的任务状态段TSS描述符和局部数据表LDT描述符
	set_tss_desc(gdt+FIRST_TSS_ENTRY,&(init_task.task.tss));
	set_ldt_desc(gdt+FIRST_LDT_ENTRY,&(init_task.task.ldt));

	/*
		清任务数组和描述符表项（注意从 i=1 开始，所以初始任务的描述符还在）。描述符项结构定义在文件 include/linux/head.h 中
		typedef struct desc_struct {
			unsigned long a,b;
		} desc_table[256];
	*/
	p = gdt+2+FIRST_TSS_ENTRY;
	for(i=1;i<NR_TASKS;i++) {
		task[i] = NULL;
		p->a=p->b=0;
		p++;
		p->a=p->b=0;
		p++;
	}
/* Clear NT, so that we won't have troubles with that later on */
	/*
		清楚EFLAGS寄存器中的NT，NT用于控制任务的嵌套调用，当NT置位时，当前中断任务执行IRET就会引起任务切换，
		NT指出TSS中的back_link字段是否有效，NT=0时无效。
	*/
	__asm__("pushfl ; andl $0xffffbfff,(%esp) ; popfl");
	/*
		将任务0的TSS段描述符地址加载到TR寄存器，局部描述符地址加载到LDTR
		详见sched.h
	*/
	ltr(0);
	lldt(0);
	/*
		初始化8253定时器
	*/
	outb_p(0x36,0x43);		/* binary, mode 3, LSB/MSB, ch 0 */
	outb_p(LATCH & 0xff , 0x40);	/* LSB */
	outb(LATCH >> 8 , 0x40);	/* MSB */
	set_intr_gate(0x20,&timer_interrupt);	// 设置时钟中断
	outb(inb_p(0x21)&~0x01,0x21);			// 修改屏蔽码，允许定时器中断
	set_system_gate(0x80,&system_call);		// 设置系统调用中断 INT 0x80
}
