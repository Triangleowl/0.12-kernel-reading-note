/*
 *  linux/kernel/exit.c
 *
 *  (C) 1991  Linus Torvalds
 */

#define DEBUG_PROC_TREE

#include <errno.h>
#include <signal.h>
#include <sys/wait.h>

#include <linux/sched.h>
#include <linux/kernel.h>
#include <linux/tty.h>
#include <asm/segment.h>

int sys_pause(void);
int sys_close(int fd);

void release(struct task_struct * p)
{
	int i;

	if (!p)
		return;
	if (p == current) {
		printk("task releasing itself\n\r");
		return;
	}
	for (i=1 ; i<NR_TASKS ; i++)
		if (task[i]==p) {		// 找到指定的任务p
			task[i]=NULL;		// 将任务p从进程数据中摘掉
			/* Update links */
			if (p->p_osptr)
				p->p_osptr->p_ysptr = p->p_ysptr;
			if (p->p_ysptr)
				p->p_ysptr->p_osptr = p->p_osptr;
			else
				p->p_pptr->p_cptr = p->p_osptr;
			free_page((long)p);
			schedule();
			return;
		}
	panic("trying to release non-existent task");
}

#ifdef DEBUG_PROC_TREE
/*
 * Check to see if a task_struct pointer is present in the task[] array
 * Return 0 if found, and 1 if not found.
 */
int bad_task_ptr(struct task_struct *p)
{
	int 	i;

	if (!p)
		return 0;
	for (i=0 ; i<NR_TASKS ; i++)
		if (task[i] == p)
			return 0;
	return 1;
}
	
/*
 * This routine scans the pid tree and make sure the rep invarient still
 * holds.  Used for debugging only, since it's very slow....
 *
 * It looks a lot scarier than it really is.... we're doing �nothing more
 * than verifying the doubly-linked list found�in p_ysptr and p_osptr, 
 * and checking it corresponds with the process tree defined by p_cptr and 
 * p_pptr;
 */
void audit_ptree()
{
	int	i;

	for (i=1 ; i<NR_TASKS ; i++) {
		if (!task[i])
			continue;
		if (bad_task_ptr(task[i]->p_pptr))
			printk("Warning, pid %d's parent link is bad\n",
				task[i]->pid);
		if (bad_task_ptr(task[i]->p_cptr))
			printk("Warning, pid %d's child link is bad\n",
				task[i]->pid);
		if (bad_task_ptr(task[i]->p_ysptr))
			printk("Warning, pid %d's ys link is bad\n",
				task[i]->pid);
		if (bad_task_ptr(task[i]->p_osptr))
			printk("Warning, pid %d's os link is bad\n",
				task[i]->pid);
		if (task[i]->p_pptr == task[i])
			printk("Warning, pid %d parent link points to self\n");
		if (task[i]->p_cptr == task[i])
			printk("Warning, pid %d child link points to self\n");
		if (task[i]->p_ysptr == task[i])
			printk("Warning, pid %d ys link points to self\n");
		if (task[i]->p_osptr == task[i])
			printk("Warning, pid %d os link points to self\n");
		if (task[i]->p_osptr) {
			if (task[i]->p_pptr != task[i]->p_osptr->p_pptr)
				printk(
			"Warning, pid %d older sibling %d parent is %d\n",
				task[i]->pid, task[i]->p_osptr->pid,
				task[i]->p_osptr->p_pptr->pid);
			if (task[i]->p_osptr->p_ysptr != task[i])
				printk(
		"Warning, pid %d older sibling %d has mismatched ys link\n",
				task[i]->pid, task[i]->p_osptr->pid);
		}
		if (task[i]->p_ysptr) {
			if (task[i]->p_pptr != task[i]->p_ysptr->p_pptr)
				printk(
			"Warning, pid %d younger sibling %d parent is %d\n",
				task[i]->pid, task[i]->p_osptr->pid,
				task[i]->p_osptr->p_pptr->pid);
			if (task[i]->p_ysptr->p_osptr != task[i])
				printk(
		"Warning, pid %d younger sibling %d has mismatched os link\n",
				task[i]->pid, task[i]->p_ysptr->pid);
		}
		if (task[i]->p_cptr) {
			if (task[i]->p_cptr->p_pptr != task[i])
				printk(
			"Warning, pid %d youngest child %d has mismatched parent link\n",
				task[i]->pid, task[i]->p_cptr->pid);
			if (task[i]->p_cptr->p_ysptr)
				printk(
			"Warning, pid %d youngest child %d has non-NULL ys link\n",
				task[i]->pid, task[i]->p_cptr->pid);
		}
	}
}
#endif /* DEBUG_PROC_TREE */

static inline int send_sig(long sig,struct task_struct * p,int priv)
{
	if (!p)
		return -EINVAL;
	if (!priv && (current->euid!=p->euid) && !suser())	// 如果当前进程的euid和指定进程的euid不同，且当前进程不是root用户，则不允许发送信号
		return -EPERM;
	if ((sig == SIGKILL) || (sig == SIGCONT)) {
		if (p->state == TASK_STOPPED)	// 如果进程处于STOPPED状态，则将状态修改为RUNNING
			p->state = TASK_RUNNING;
		p->exit_code = 0;
		p->signal &= ~( (1<<(SIGSTOP-1)) | (1<<(SIGTSTP-1)) |
				(1<<(SIGTTIN-1)) | (1<<(SIGTTOU-1)) );		// 过滤掉 SIGSTOP、SIGTSTP、SIGTTIN 和 SIGTTOU
	} 
	/* If the signal will be ignored, don't even post it */
	if ((int) p->sigaction[sig-1].sa_handler == 1)			// 如果进程p对sig信号的处理方式是忽略，则不需要发送:)
		return 0;
	/* Depends on order SIGSTOP, SIGTSTP, SIGTTIN, SIGTTOU */
	if ((sig >= SIGSTOP) && (sig <= SIGTTOU))	// SIGSTOP=19、SIGTSTP=20、SIGTTIN=21、SIGTTOU=22
		p->signal &= ~(1<<(SIGCONT-1));			// 如果信号是上述四种信号之一，则将信号位图中的SIGCONT(继续运行信号)位清零
	/* Actually deliver the signal */
	p->signal |= (1<<(sig-1));		// 向进程p发送信号sig
	return 0;
}

int session_of_pgrp(int pgrp)	// 返回进程组号为pgrp的进程的会话号
{
	struct task_struct **p;

 	for (p = &LAST_TASK ; p > &FIRST_TASK ; --p)
		if ((*p)->pgrp == pgrp)
			return((*p)->session);
	return -1;
}

int kill_pg(int pgrp, int sig, int priv)	// 向进程组pgrp的【所有】进程发送sig信号
{
	struct task_struct **p;
	int err,retval = -ESRCH;
	int found = 0;

	if (sig<1 || sig>32 || pgrp<=0)
		return -EINVAL;
 	for (p = &LAST_TASK ; p > &FIRST_TASK ; --p)
		if ((*p)->pgrp == pgrp) {
			if (sig && (err = send_sig(sig,*p,priv)))
				retval = err;
			else
				found++;
		}
	return(found ? 0 : retval);
}

int kill_proc(int pid, int sig, int priv)	// 向指定的进程发送sig信号
{
 	struct task_struct **p;

	if (sig<1 || sig>32)
		return -EINVAL;
	for (p = &LAST_TASK ; p > &FIRST_TASK ; --p)
		if ((*p)->pid == pid)
			return(sig ? send_sig(sig,*p,priv) : 0);
	return(-ESRCH);
}

/*
 * POSIX specifies that kill(-1,sig) is unspecified, but what we have
 * is probably wrong.  Should make it like BSD or SYSV.
 */
int sys_kill(int pid,int sig)
{
	struct task_struct **p = NR_TASKS + task;
	int err, retval = 0;

	if (!pid)			// pid == 0，发送给与当前进属于同一个组的所有进程
		return(kill_pg(current->pid,sig,0));
	if (pid == -1) {	// pid == -1，发送给pid>1的所有进程
		while (--p > &FIRST_TASK)
			if (err = send_sig(sig,*p,0))
				retval = err;
		return(retval);
	}
	if (pid < 0) 		// pid < 0 && pid != -1，发送给与pid在同一个组的所有进程
		return(kill_pg(-pid,sig,0));
	/* Normal kill */
	return(kill_proc(pid,sig,0));	// pid > 0，只给指定进程发送信号
}

/*
 * Determine if a process group is "orphaned", according to the POSIX
 * definition in 2.2.2.52.  Orphaned process groups are not to be affected
 * by terminal-generated stop signals.  Newly orphaned process groups are 
 * to receive a SIGHUP and a SIGCONT.
 * 
 * "I ask you, have you ever known what it is to be an orphan?"
 */
int is_orphaned_pgrp(int pgrp)	// 寻找指定进程组pgrp是否是孤儿进程组，如果是返回1，否则返回0
{
	struct task_struct **p;

	for (p = &LAST_TASK ; p > &FIRST_TASK ; --p) {
		if (!(*p) ||							// 跳过空任务
		    ((*p)->pgrp != pgrp) || 			// 跳过不属于pgrp组的进程
		    ((*p)->state == TASK_ZOMBIE) ||		// 跳过僵尸进程
		    ((*p)->p_pptr->pid == 1))			// 跳过父进程是init的进程
			continue;
		if (((*p)->p_pptr->pgrp != pgrp) &&				// 如果某个进程的父进程不属于pgrp
		    ((*p)->p_pptr->session == (*p)->session))	// 并且父进程和当前进程属于同一个session，则说明这个进程组肯定不是孤儿进程组
			return 0;
	}
	return(1);	/* (sighing) "Often!" */
}

static int has_stopped_jobs(int pgrp)	// 判断指定的进程组pgrp种是否有处于STOPPED的进程，如果有返回1，无则返回0
{
	struct task_struct ** p;

	for (p = &LAST_TASK ; p > &FIRST_TASK ; --p) {
		if ((*p)->pgrp != pgrp)
			continue;
		if ((*p)->state == TASK_STOPPED)
			return(1);
	}
	return(0);
}

/*
	执行程序善后工作，如
	- 释放code/data segment的内存；
	- 所有用到的文件的引用计数减1;
	- 判断当前进程的结束是否会导致同组的进程称为孤儿组的可能，如果有做相应的处理;
	- 将当前进程的所有子进程过继给init;
	- 设置进程结束码;
*/
volatile void do_exit(long code)	// 程序退出处理函数
{
	struct task_struct *p;
	int i;
	/*
		释放当前进程code segment和data segment使用的内存页，
		get_base()返回当前进程的基址，get_limit()返回limit
	*/
	free_page_tables(get_base(current->ldt[1]),get_limit(0x0f));
	free_page_tables(get_base(current->ldt[2]),get_limit(0x17));
	for (i=0 ; i<NR_OPEN ; i++)		// 关闭所有打开的文件
		if (current->filp[i])
			sys_close(i);
	iput(current->pwd);				// 对进程的工作路径的引用减1
	current->pwd = NULL;
	iput(current->root);			// 同上
	current->root = NULL;
	iput(current->executable);		// 同上
	current->executable = NULL;
	iput(current->library);			// 同上
	current->library = NULL;
	current->state = TASK_ZOMBIE;	// 进程状态设置为TASK_ZOMBIE，此进程再也不会被运行了，一路走好:(
	current->exit_code = code;		// 设置退出码
	/* 
	 * Check to see if any process groups have become orphaned
	 * as a result of our exiting, and if they have any stopped
	 * jobs, send them a SIGUP and then a SIGCONT.  (POSIX 3.2.2.2)
	 *
	 * Case i: Our father is in a different pgrp than we are
	 * and we were the only connection outside, so our pgrp
	 * is about to become orphaned.
 	 */
	if ((current->p_pptr->pgrp != current->pgrp) &&
	    (current->p_pptr->session == current->session) &&
	    is_orphaned_pgrp(current->pgrp) &&		// 判断当前进程退出是否会导致当前进程组编程孤儿进程组，如果是，并且
	    has_stopped_jobs(current->pgrp)) {		// 当前进程组中有进程处于STOPPED的进程，则向他们发送SIGUP和SIGCONT信号
		kill_pg(current->pgrp,SIGHUP,1);		
		kill_pg(current->pgrp,SIGCONT,1);
	}
	/* Let father know we died */
	current->p_pptr->signal |= (1<<(SIGCHLD-1));	// 通知父进程自己已经离开
	
	/*
	 * This loop does two things:
	 * 
  	 * A.  Make init inherit all the child processes					// 让init继承所有子进程
	 * B.  Check to see if any process groups have become orphaned		// 
	 *	as a result of our exiting, and if they have any stopped
	 *	jons, send them a SIGUP and then a SIGCONT.  (POSIX 3.2.2.2)
	 */
	if (p = current->p_cptr) {	// 当前进程存在子进程，p_cptr指向最新创建的子进程
		while (1) {
			p->p_pptr = task[1];	// 子进程的p_pptr指向init进程
			if (p->state == TASK_ZOMBIE)	// 如果子进程处于ZOMBIE状态，则向父进程，即init发送SIGCHLD信号
				task[1]->signal |= (1<<(SIGCHLD-1));
			/*
			 * process group orphan check
			 * Case ii: Our child is in a different pgrp 
			 * than we are, and it was the only connection
			 * outside, so the child pgrp is now orphaned.
			 */
			if ((p->pgrp != current->pgrp) &&
			    (p->session == current->session) &&
			    is_orphaned_pgrp(p->pgrp) &&
			    has_stopped_jobs(p->pgrp)) {
				kill_pg(p->pgrp,SIGHUP,1);
				kill_pg(p->pgrp,SIGCONT,1);
			}
			if (p->p_osptr) {
				p = p->p_osptr;
				continue;
			}
			/*
			 * This is it; link everything into init's children 
			 * and leave 
			 */
			p->p_osptr = task[1]->p_cptr;
			task[1]->p_cptr->p_ysptr = p;
			task[1]->p_cptr = current->p_cptr;
			current->p_cptr = 0;
			break;
		}
	}
	if (current->leader) {		// 如果当前进程是session的leader，则向所有进程发送SIGHUP信号
		struct task_struct **p;
		struct tty_struct *tty;

		if (current->tty >= 0) {
			tty = TTY_TABLE(current->tty);
			if (tty->pgrp>0)
				kill_pg(tty->pgrp, SIGHUP, 1);
			tty->pgrp = 0;
			tty->session = 0;
		}
	 	for (p = &LAST_TASK ; p > &FIRST_TASK ; --p)
			if ((*p)->session == current->session)
				(*p)->tty = -1;
	}
	if (last_task_used_math == current)
		last_task_used_math = NULL;
#ifdef DEBUG_PROC_TREE
	audit_ptree();
#endif
	schedule();		// 重新调度其他进程，当前进程已经彻底over，不会再有它什么事了
}

int sys_exit(int error_code)
{
	do_exit((error_code&0xff)<<8);
}

/*
	waitpid系统调用，挂起当前进程，直到pid指定的进程退出或者收到要求终止该进程的信号
	pid > 0: 等待 进程号等于pid的子进程结束
	pid = 0: 等待 进程组号等于当前进程组号的任何子进程
	pid < -1: 等待进程组号等于 -pid 的任何子进程
	pid = -1: 等待任何子进程
	若 options = WUNTRACED，表示如果子进程是停止的，也马上返回（无须跟踪）。
	若 options = WNOHANG，表示如果没有子进程退出或终止就马上返回。
	如果返回状态指针 stat_addr 不为空，则就将状态信息保存到那里。
	参数 pid 是进程号；*stat_addr 是保存状态信息位置的指针；options 是 waitpid 选项。
*/
int sys_waitpid(pid_t pid,unsigned long * stat_addr, int options)
{
	int flag;
	struct task_struct *p;
	unsigned long oldblocked;

	verify_area(stat_addr,4); // 验证存放信息的地址空间是否足够
repeat:
	flag=0;
	for (p = current->p_cptr ; p ; p = p->p_osptr) {
		if (pid>0) {
			if (p->pid != pid)
				continue;
		} else if (!pid) {
			if (p->pgrp != current->pgrp)
				continue;
		} else if (pid != -1) {
			if (p->pgrp != -pid)
				continue;
		}
		switch (p->state) {
			case TASK_STOPPED:
				if (!(options & WUNTRACED) || 
				    !p->exit_code)
					continue;
				put_fs_long((p->exit_code << 8) | 0x7f,
					stat_addr);
				p->exit_code = 0;
				return p->pid;
			case TASK_ZOMBIE:
				current->cutime += p->utime;
				current->cstime += p->stime;
				flag = p->pid;
				put_fs_long(p->exit_code, stat_addr);
				release(p);
#ifdef DEBUG_PROC_TREE
				audit_ptree();
#endif
				return flag;
			default:
				flag=1;
				continue;
		}
	}
	if (flag) {
		if (options & WNOHANG)
			return 0;
		current->state=TASK_INTERRUPTIBLE;
		oldblocked = current->blocked;
		current->blocked &= ~(1<<(SIGCHLD-1));
		schedule();
		current->blocked = oldblocked;
		if (current->signal & ~(current->blocked | (1<<(SIGCHLD-1))))
			return -ERESTARTSYS;
		else
			goto repeat;
	}
	return -ECHILD;
}


