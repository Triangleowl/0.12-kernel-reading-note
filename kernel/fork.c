/*
 *  linux/kernel/fork.c
 *
 *  (C) 1991  Linus Torvalds
 */

/*
 *  'fork.c' contains the help-routines for the 'fork' system call
 * (see also system_call.s), and some misc functions ('verify_area').
 * Fork is rather simple, once you get the hang of it, but the memory
 * management can be a bitch. See 'mm/mm.c': 'copy_page_tables()'
 */
#include <errno.h>

#include <linux/sched.h>
#include <linux/kernel.h>
#include <asm/segment.h>
#include <asm/system.h>

extern void write_verify(unsigned long address);

long last_pid=0;

void verify_area(void * addr,int size)
{
	unsigned long start;

	start = (unsigned long) addr;
	size += start & 0xfff;
	start &= 0xfffff000;
	start += get_base(current->ldt[2]);
	while (size>0) {
		size -= 4096;
		write_verify(start);
		start += 4096;
	}
}

int copy_mem(int nr,struct task_struct * p)
{
	unsigned long old_data_base,new_data_base,data_limit;
	unsigned long old_code_base,new_code_base,code_limit;

	code_limit=get_limit(0x0f);
	data_limit=get_limit(0x17);
	old_code_base = get_base(current->ldt[1]);
	old_data_base = get_base(current->ldt[2]);
	if (old_data_base != old_code_base)
		panic("We don't support separate I&D");
	if (data_limit < code_limit)
		panic("Bad data_limit");
	new_data_base = new_code_base = nr * TASK_SIZE;
	p->start_code = new_code_base;
	set_base(p->ldt[1],new_code_base);
	set_base(p->ldt[2],new_data_base);
	if (copy_page_tables(old_data_base,new_data_base,data_limit)) {
		free_page_tables(new_data_base,data_limit);
		return -ENOMEM;
	}
	return 0;
}

/*
 *  Ok, this is the main fork-routine. It copies the system process
 * information (task[nr]) and sets up the necessary registers. It
 * also copies the data segment in it's entirety.
 */
// 第一个参数nr是_sys_fork调用_find_empty_process找到的empty_process在task数组中的索引
int copy_process(int nr,long ebp,long edi,long esi,long gs,long none,
		long ebx,long ecx,long edx, long orig_eax, 
		long fs,long es,long ds,
		long eip,long cs,long eflags,long esp,long ss)
{
	struct task_struct *p;
	int i;
	struct file *f;

	p = (struct task_struct *) get_free_page();
	if (!p)
		return -EAGAIN;
	task[nr] = p;	// task是记录任务的数组，nr是任务的编号，也是pid
	/*
		下面是直接将current的进程结构体的信息整个复制到p中
	*/
	*p = *current;	/* NOTE! this doesn't copy the supervisor stack */

	// 下面是对p的进程结构体做一些调整，子进程不能全盘照抄父进程的:)

	// 首先是修改状态、pid等信息
	p->state = TASK_UNINTERRUPTIBLE;
	p->pid = last_pid;
	p->counter = p->priority;	// 运行时间片值
	p->signal = 0;				// 信号位图
	p->alarm = 0;				// 报警定时器
	p->leader = 0;				/* process leadership doesn't inherit */
	p->utime = p->stime = 0;	// 用户态/内核态运行时间
	p->cutime = p->cstime = 0;	// 子进程用户态/内核态运行时间
	p->start_time = jiffies;	// 进程开始的时间

	// 接着修改任务状态段TSS内容
	p->tss.back_link = 0;
	p->tss.esp0 = PAGE_SIZE + (long) p;	// 任务内核态栈指针
	p->tss.ss0 = 0x10;					// 内核态栈的段选择子
	p->tss.eip = eip;					// eip
	p->tss.eflags = eflags;
	p->tss.eax = 0;
	p->tss.ecx = ecx;
	p->tss.edx = edx;
	p->tss.ebx = ebx;
	p->tss.esp = esp;
	p->tss.ebp = ebp;
	p->tss.esi = esi;
	p->tss.edi = edi;
	p->tss.es = es & 0xffff;	// 段选择子仅低16位有效
	p->tss.cs = cs & 0xffff;
	p->tss.ss = ss & 0xffff;
	p->tss.ds = ds & 0xffff;
	p->tss.fs = fs & 0xffff;
	p->tss.gs = gs & 0xffff;
	p->tss.ldt = _LDT(nr);		// 任务LDT描述符的段选择子（LDT描述符在GDT中哦）
	p->tss.trace_bitmap = 0x80000000;
	if (last_task_used_math == current)
		__asm__("clts ; fnsave %0 ; frstor %0"::"m" (p->tss.i387));
	if (copy_mem(nr,p)) {		// 返回非0表示出错
		task[nr] = NULL;
		free_page((long) p);
		return -EAGAIN;
	}
	// 子进程会继承父进程打开的文件，遍历父进程打开的文件，将其ref加1
	for (i=0; i<NR_OPEN;i++)
		if (f=p->filp[i])
			f->f_count++;
	if (current->pwd)
		current->pwd->i_count++;
	if (current->root)
		current->root->i_count++;
	if (current->executable)
		current->executable->i_count++;
	if (current->library)
		current->library->i_count++;
	/*
	gdt是一个desc_struct结构体，见head.h，其定义在head.s最后几行
	typedef struct desc_struct {
		unsigned long a,b;
	} desc_table[256];
	nr乘2(左移1位)是因为每个进程需要占两个段描述符(tss和ldt)
	*/
	set_tss_desc(gdt+(nr<<1)+FIRST_TSS_ENTRY,&(p->tss));
	set_ldt_desc(gdt+(nr<<1)+FIRST_LDT_ENTRY,&(p->ldt));
	p->p_pptr = current;	// 自己成的父进程位当前进程
	p->p_cptr = 0;			// 子进程的最新子进程指针为0
	p->p_ysptr = 0;			// 
	p->p_osptr = current->p_cptr;
	if (p->p_osptr)
		p->p_osptr->p_ysptr = p;
	current->p_cptr = p;
	p->state = TASK_RUNNING;	/* do this last, just in case */
	return last_pid;		// 返回子进程的pid，用户态的进程从eax中读取返回值，所以子进程的返回值为0
}

int find_empty_process(void)
{
	int i;

	repeat:
		if ((++last_pid)<0) last_pid=1;
		for(i=0 ; i<NR_TASKS ; i++)
			if (task[i] && ((task[i]->pid == last_pid) ||
				        (task[i]->pgrp == last_pid)))
				goto repeat;
	for(i=1 ; i<NR_TASKS ; i++)
		if (!task[i])
			return i;
	return -EAGAIN;
}
