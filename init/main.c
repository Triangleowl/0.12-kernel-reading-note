/*
 *  linux/init/main.c
 *
 *  (C) 1991  Linus Torvalds
 */

#define __LIBRARY__
#include <unistd.h>
#include <time.h>

/*
 * we need this inline - forking from kernel space will result
 * in NO COPY ON WRITE (!!!), until an execve is executed. This
 * is no problem, but for the stack. This is handled by not letting
 * main() use the stack at all after fork(). Thus, no function
 * calls - which means inline code for fork too, as otherwise we
 * would use the stack upon exit from 'fork()'.
 *
 * Actually only pause and fork are needed inline, so that there
 * won't be any messing with the stack from main(), but we define
 * some others too.
 */

/*
	_syscallN 是宏定义，在unistd.h中，N表示参数的个数(即0表示不接收参数)
*/
static inline _syscall0(int,fork)
static inline _syscall0(int,pause)
static inline _syscall1(int,setup,void *,BIOS)
static inline _syscall0(int,sync)

#include <linux/tty.h>
#include <linux/sched.h>
#include <linux/head.h>
#include <asm/system.h>
#include <asm/io.h>

#include <stddef.h>
#include <stdarg.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>

#include <linux/fs.h>

#include <string.h>

static char printbuf[1024];

extern char *strcpy();
extern int vsprintf();
extern void init(void);
extern void blk_dev_init(void);
extern void chr_dev_init(void);
extern void hd_init(void);
extern void floppy_init(void);
extern void mem_init(long start, long end);
extern long rd_init(long mem_start, int length);
extern long kernel_mktime(struct tm * tm);

static int sprintf(char * str, const char *fmt, ...)
{
	va_list args;
	int i;

	va_start(args, fmt);
	i = vsprintf(str, fmt, args);
	va_end(args);
	return i;
}

/*
 * This is set up by the setup-routine at boot-time
 */
#define EXT_MEM_K (*(unsigned short *)0x90002)
#define CON_ROWS ((*(unsigned short *)0x9000e) & 0xff)
#define CON_COLS (((*(unsigned short *)0x9000e) & 0xff00) >> 8)
#define DRIVE_INFO (*(struct drive_info *)0x90080)
#define ORIG_ROOT_DEV (*(unsigned short *)0x901FC)
#define ORIG_SWAP_DEV (*(unsigned short *)0x901FA)

/*
 * Yeah, yeah, it's ugly, but I cannot find how to do this correctly
 * and this seems to work. I anybody has more info on the real-time
 * clock I'd be interested. Most of this was trial and error, and some
 * bios-listing reading. Urghh.
 */

#define CMOS_READ(addr) ({ \
outb_p(0x80|addr,0x70); \
inb_p(0x71); \
})

#define BCD_TO_BIN(val) ((val)=((val)&15) + ((val)>>4)*10)

static void time_init(void)
{
	struct tm time;

	do {
		time.tm_sec = CMOS_READ(0);
		time.tm_min = CMOS_READ(2);
		time.tm_hour = CMOS_READ(4);
		time.tm_mday = CMOS_READ(7);
		time.tm_mon = CMOS_READ(8);
		time.tm_year = CMOS_READ(9);
	} while (time.tm_sec != CMOS_READ(0));
	BCD_TO_BIN(time.tm_sec);
	BCD_TO_BIN(time.tm_min);
	BCD_TO_BIN(time.tm_hour);
	BCD_TO_BIN(time.tm_mday);
	BCD_TO_BIN(time.tm_mon);
	BCD_TO_BIN(time.tm_year);
	time.tm_mon--;
	startup_time = kernel_mktime(&time);
}

static long memory_end = 0;
static long buffer_memory_end = 0;
static long main_memory_start = 0;
static char term[32];

static char * argv_rc[] = { "/bin/sh", NULL };
static char * envp_rc[] = { "HOME=/", NULL ,NULL };

static char * argv[] = { "-/bin/sh",NULL };
static char * envp[] = { "HOME=/usr/root", NULL, NULL };

struct drive_info { char dummy[32]; } drive_info;

void main(void)		/* This really IS void, no error here. */
{			/* The startup routine assumes (well, ...) this */
/*
 * Interrupts are still disabled. Do necessary setups, then
 * enable them
 */
/*
	！！！！！！此时处于关中断的状态
*/
/*
	保存setup.S保存在0x900xx处的系统参数信息(根文件系统设备号、交换文件设备号、终端屏幕参数等)
	ROOT_DEV定义在super.c
	SWAP_DEV定义在swap.c

*/
 	ROOT_DEV = ORIG_ROOT_DEV;
 	SWAP_DEV = ORIG_SWAP_DEV;
	sprintf(term, "TERM=con%dx%d", CON_COLS, CON_ROWS);
	envp[1] = term;	
	envp_rc[1] = term;
 	drive_info = DRIVE_INFO;
	
	/*
		memory_end表示内存的大小(以字节为单位)
	*/
	memory_end = (1<<20) + (EXT_MEM_K<<10);		// 1MB + EXT+MEM_K * 1024 bytes
	memory_end &= 0xfffff000;					// 内存凑够page大小的整数倍(4KB)
	if (memory_end > 16*1024*1024)				// 如果内存大小超过16M，则只使用16MB
		memory_end = 16*1024*1024;
	if (memory_end > 12*1024*1024) 				// 如果内存大于12MB，则缓冲区末端为4M
		buffer_memory_end = 4*1024*1024;
	else if (memory_end > 6*1024*1024)			// 如果内存大于6MB，则缓冲区末端为2MB
		buffer_memory_end = 2*1024*1024;
	else
		buffer_memory_end = 1*1024*1024;		// 如果小于6M，则缓冲区末端为1MB
	main_memory_start = buffer_memory_end;		// 主存开始的地方，缓冲区结束的地方
/*
	如果Makefile中定义了RAMDISK，则主存要往上移动
*/
#ifdef RAMDISK
	main_memory_start += rd_init(main_memory_start, RAMDISK*1024);
#endif
	/*
		主内存初始化，在mm/memory.c
		mem_map[]数组用于记录内存是否已被进程占用，mem_init将所有的主内存标记为未使用。
	*/
	mem_init(main_memory_start,memory_end);
	/*
		陷阱门(硬件中断向量)初始化，在kernel/traps.c
		设置各种中断的中断服务程序，将IDTE设置成响应的值而已
	*/
	trap_init();
	/*
		块设备初始化，blk_drv/ll_rw_blk.c
	*/
	blk_dev_init();
	/*
		字符设备初始化，chr_drv/tty_io.c
	*/
	chr_dev_init();
	/*
		tty初始化， chr_drv/tty_io.c
	*/
	tty_init();
	/*
		设置开机时间，time_init()在本文件中
	*/
	time_init();
	/*
		调度程序初始化，最后一步是设置 system_call 的 interrupt gate 描述符
	*/
	sched_init();
	/*
		缓冲区管理初始化
	*/
	buffer_init(buffer_memory_end);
	/*
		硬盘初始化
	*/
	hd_init();
	/*
		软驱初始化
	*/
	floppy_init();
	/*
		开启中断
	*/
	sti();
	/*
		include/asm/system.h
	*/
	move_to_user_mode(); // 此函数执行完成后CPL=3，不是最高的特权级了
	if (!fork()) {		/* we count on this going ok */
		// 子进程调用init，父进程不会进来
		init();
	}
/*
 *   NOTE!!   For any other task 'pause()' would mean we have to get a
 * signal to awaken, but task0 is the sole exception (see 'schedule()')
 * as task 0 gets activated at every idle moment (when no other tasks
 * can run). For task0 'pause()' just means we go check if some other
 * task can run, and if not we return here.
 */
	for(;;)
		__asm__("int $0x80"::"a" (__NR_pause):"ax");
}

static int printf(const char *fmt, ...)
{
	va_list args;
	int i;

	va_start(args, fmt);
	write(1,printbuf,i=vsprintf(printbuf, fmt, args));
	va_end(args);
	return i;
}

void init(void)
{
	int pid,i;

	// setup是一个系统调用，用于读取硬盘参数并加载虚拟盘和安装根文件系统设备，实现见kernel/blk_drv/hd.c
	setup((void *) &drive_info);
	(void) open("/dev/tty1",O_RDWR,0); // 打开终端控制台，第一次打开，所以分配的文件描述符为0，即stdin
	(void) dup(0);	// 复制文件描述符0，得到文件描述符1，即stdout
	(void) dup(0);	// 同理，得到文件描述符2，即stderr
	// NR_BUFFERS: 缓冲区块数
	// BLOCK_SIZE: 缓冲区大小
	printf("%d buffers = %d bytes buffer space\n\r",NR_BUFFERS,
		NR_BUFFERS*BLOCK_SIZE);
	// memory_end-main_memory_start: 主内存去内的空闲内存(字节为单位)
	printf("Free mem: %d bytes\n\r",memory_end-main_memory_start);
	if (!(pid=fork())) {
		close(0);	// 关闭stdin，则下面open打开/etc/rc得到的文件描述符是0，即将stdin重定向到/ect/rc中
		if (open("/etc/rc",O_RDONLY,0))
			_exit(1);
		// 让/bin/sh执行/ect/rc中的命令，注意这里sh是非交互方式运行的，所以执行完/ect/rc中的命令后就会退出
		execve("/bin/sh",argv_rc,envp_rc);	
		_exit(2);
	}
	if (pid>0)
		while (pid != wait(&i))	// 等待上面创建的那个子进程结束
			/* nothing */;
	while (1) {
		if ((pid=fork())<0) {
			printf("Fork failed in init\r\n");
			continue;
		}
		if (!pid) {				// 子进程运行/bin/sh
			close(0);close(1);close(2);
			setsid();
			(void) open("/dev/tty1",O_RDWR,0);
			(void) dup(0);		// 复制文件描述符0，见fs/fcntl.c:sys_dup()
			(void) dup(0);		// 同上
			_exit(execve("/bin/sh",argv,envp));
		}
		while (1)				// 父进程等待/bin/sh子进程结束
			if (pid == wait(&i))
				break;			// 如果/bin/sh子进程结束，则再重新创建一个
		printf("\n\rchild %d died with code %04x\n\r",pid,i);
		sync();
	}
	// _exit和exit是有区别的，_exit是sys_exit系统调用，exit是普通库函数。
	// exit会先执行清理操作，然后再调用sys_exit
	_exit(0);	/* NOTE! _exit, not exit() */
}
