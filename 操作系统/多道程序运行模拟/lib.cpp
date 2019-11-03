#include "stdafx.h"

#include "FScpp.h"

#include <cstdio>
#include <cstdlib>

struct progress
{
	int runtime1;
	int iotime;
	int runtime2;
};

namespace FScpp
{
	FS::FS()
	{
		int count;
		int i = 0, flag = 0, flag2 = 0, rest[100];
		const auto time = 0;
		struct progress f[100], s[100];
		printf("请输入进程个数（2-24）并且按照优先级从高到低依次输入\n");
		scanf("%d", &count);
		while (i < count)
		{
			printf("进程计算时长1,I/O操作时长,进程计算时长");
			scanf("%d", &f[i].runtime1);
			scanf("%d", &f[i].iotime);
			scanf("%d", &f[i].runtime2);
			i++;
		}
		printf("----------多道抢占式调度算法----------\n");
		for (i = 0; i < count; i++)
		{
			//此处是第一的程序的执行情况 
			if (flag == 0)
			{
				s[i].runtime1 = time + f[i].runtime1;
				printf("从%dms到%dms程序%d在CPU上运行\n", time, s[i].runtime1, i);
				s[i].iotime = s[i].runtime1 + f[i].iotime;
				printf("从%dms到%dms程序%d在I/O上运行\n", s[i].runtime1, s[i].iotime, i);
				s[i].runtime2 = s[i].iotime + f[i].runtime2;
				printf("从%dms到%dms程序%d进行CPU操作\n", s[i].iotime, s[i].runtime2, i);
				flag = 1;
			}
			//接下来是出去第一个进程的运行情况 
			else
			{
				if (f[i].runtime1 > f[i - 1].iotime&&flag2 == 0)
				{
					s[i].runtime1 = s[i - 1].runtime1 + f[i - 1].iotime;
					printf("从%dms到%dms程序%d在CPU上运行,此时%d的CPU运算与%d的I/O同时运行\n", s[i - 1].runtime1, s[i].runtime1, i, i, i - 1);
					rest[i] = f[i].runtime1 - f[i - 1].iotime;
					s[i].runtime1 = s[i - 1].runtime2 + rest[i];
					printf("从%dms到%dms程序%d在CPU上运行\n", s[i - 1].runtime2, s[i].runtime1, i);
					s[i].iotime = s[i].runtime1 + f[i].iotime;
					printf("从%dms到%dms程序%d进行I/O操作\n", s[i].runtime1, s[i].iotime, i);
					s[i].runtime2 = s[i].iotime + f[i].runtime2;
					printf("从%dms到%dms程序%d进行CPU操作\n", s[i].iotime, s[i].runtime2, i);
				}
				else if (f[i].runtime1 <= f[i - 1].iotime&&flag2 == 0)
				{
					s[i].runtime1 = s[i - 1].runtime1 + f[i].runtime1;
					printf("从%dms到%dms程序%d在CPU上运行，此时%d的CPU运算与%d的I/O同时运行\n", s[i - 1].runtime1, s[i].runtime1, i, i, i - 1);
					s[i].iotime = s[i - 1].iotime + f[i].iotime;
					printf("从%dms到%dms程序%d进行I/O操作,此时%d的CPU运算与%d的I/O同时运行\n", s[i - 1].iotime, s[i].iotime, i, i - 1, i);
					s[i].runtime2 = s[i].iotime + f[i].runtime2;
					printf("从%dms到%dms程序%d进行CPU操作\n", s[i].iotime, s[i].runtime2, i);
					rest[i] = f[i - 1].iotime - f[i].runtime1;
					flag2 = 1;
				}
				else if (rest[i - 1] < f[i].runtime1&&flag2 != 0)
				{
					s[i].runtime1 = s[i - 1].runtime1 + rest[i - 1];
					printf("从%dms到%dms程序%d进行CPU操作\n", s[i - 1].runtime1, s[i].runtime1, i);
					rest[i] = f[i].runtime1 - rest[i - 1];
					if (rest[i - 1] >= s[i].runtime1)
					{
						printf("此时%d的CPU运算与%d的I/O同时运行", i, i - 3);
					}
					else
					{
						printf("此时%d的CPU运算与%d的I/O同时运行", i, i - 2);
					}
					if (f[i - 3].iotime - f[i - 2].runtime1 - f[i - 1].runtime1 - rest[i - 1] >= 0)
					{
						s[i].runtime1 = s[i - 3].runtime2 + rest[i];
						printf("从%dms到%dms程序%d进行CPU操作,此时%d的CPU运算与%d的I/O同时运行\n", s[i - 3].runtime2, s[i].runtime1, i, i, i - 2);
					}
					else
					{
						s[i].runtime1 = s[i - 2].runtime2 + rest[i];
						printf("从%dms到%dms程序%d进行CPU操作,此时%d的CPU运算与%d的I/O同时运行\n", s[i - 2].runtime2, s[i].runtime1, i, i, i - 1);
					}
					if (s[i].runtime1 > s[i - 1].iotime)
					{
						int a = s[i].runtime1 - s[i - 1].iotime;
						printf("从%dms到%dms程序%d进行CPU操作,此时%d的CPU运算与%d的I/O同时运行\n", s[i - 2].runtime2, s[i - 1].iotime, i, i, i - 1);
						s[i].runtime1 = s[i - 1].runtime2 + a;
						printf("从%dms到%dms程序%d进行CPU操作\n", s[i - 1].runtime2, s[i].runtime1, i);
						s[i].iotime = s[i].runtime1 + f[i].iotime;
						printf("从%dms到%dms程序%d进行I/O操作\n", s[i].runtime1, s[i].iotime, i);
						if (f[i - 1].runtime2 > f[i].iotime)
						{
							s[i].runtime2 = s[i - 1].runtime2 + f[i].runtime2;
							printf("从%dms到%dms程序%d进行CPU操作\n", s[i - 1].runtime2, s[i].runtime2, i);
						}
						else
						{
							s[i].runtime2 = s[i].iotime + f[i].runtime2;
							printf("从%dms到%dms程序%d进行CPU操作\n", s[i].iotime, s[i].runtime2, i);
						}
						flag2 = 1;
					}
					else
					{

						s[i].iotime = s[i - 1].iotime + f[i].iotime;
						printf("从%dms到%dms程序%d进行I/O操作,此时%d的CPU运算与%d的I/O同时运行\n", s[i - 1].iotime, s[i].iotime, i, i - 1, i);
						if (f[i - 1].runtime2 > f[i].iotime)
						{
							s[i].runtime2 = s[i - 1].runtime2 + f[i].runtime2;
							printf("从%dms到%dms程序%d进行CPU操作\n", s[i - 1].runtime2, s[i].runtime2, i);
						}
						else
						{
							s[i].runtime2 = s[i].iotime + f[i].runtime2;
							printf("从%dms到%dms程序%d进行CPU操作\n", s[i].iotime, s[i].runtime2, i);
						}
						flag2 = 1;
					}
				}
				else if (rest[i - 1] > f[i].runtime1&&flag2 != 0)
				{
					s[i].runtime1 = s[i - 1].runtime1 + f[i].runtime1;
					rest[i] = rest[i - 1] - f[i].runtime1;
					printf("从%dms到%dms程序%d进行CPU操作,此时%d的CPU运算与%d的I/O同时运行\n", s[i - 1].runtime1, s[i].runtime1, i, i, i - 2);
					s[i].iotime = s[i - 1].iotime + f[i].iotime;
					printf("从%dms到%dms程序%d进行I/O操作,此时%d的CPU运算与%d的I/O同时运行\n", s[i - 1].iotime, s[i].iotime, i, i - 1, i);
					if (f[i - 1].runtime2 > f[i].iotime)
					{
						s[i].runtime2 = s[i - 1].runtime2 + f[i].runtime2;
						printf("从%dms到%dms程序%d进行CPU操作\n", s[i - 1].runtime2, s[i].runtime2, i);
					}
					else
					{
						s[i].runtime2 = s[i].iotime + f[i].runtime2;
						printf("从%dms到%dms程序%d进行CPU操作\n", s[i].iotime, s[i].runtime2, i);
					}
				}
			}
		}
		system("pause");
	}
}
