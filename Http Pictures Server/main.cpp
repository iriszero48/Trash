#include <string>
#include <valarray>
#include <sys/stat.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <err.h>
#include <map>
#include <regex>
#include <dirent.h>
#include <list>
#include <numeric>
#include <thread>
#include <arpa/inet.h>
#include <cctype>

#define threadNum 100

auto UrlEncodeTableGenerate()
{
	char html5[256] = {0};
	for (auto i = 0; i < 256; i++) html5[i] = isalnum(i) || i == '*' || i == '-' || i == '.' || i == '_'
		                                          ? i
		                                          : (i == ' ')
		                                          ? '+'
		                                          : 0;
	return html5;
}

const auto UrlEncodeTable = UrlEncodeTableGenerate();

int FileExists(const char* path)
{
	struct stat sb{};
	return !stat(path, &sb) && S_ISREG(sb.st_mode);
}

int DirectoryExists(const char* path)
{
	struct stat sb{};
	return !stat(path, &sb) && S_ISDIR(sb.st_mode);
}

uint64_t FileSize(const char* path)
{
	struct stat sb{};
	stat(path, &sb);
	return sb.st_size;
}

std::string encode(const char* s, uint16_t len)
{
	auto enc = new char[len * 3];
	for (; *s; s++)
	{
		if (UrlEncodeTable[*s]) sprintf(enc, "%c", UrlEncodeTable[*s]);
		else sprintf(enc, "%%%02X", *s);
		while (*++enc);
	}
	auto r = std::string(enc);
	delete[] enc;
	return r;
}


std::string UrlDecode(const char* s, uint16_t len)
{
	char* dec = new char[len + 1];
	const char* end = s + strlen(s);
	int c;
	for (char* o = dec; s <= end; o++)
	{
		c = *s++;
		if (c == '+') c = ' ';
		else if (c == '%')
		{
			*s++;
			*s++;
			sscanf(s - 2, "%2x", &c);
		}
		*o = c;
	}
	auto r = std::string(dec, strlen(dec));
	delete[] dec;
	return r;
}

void GetFiles(const char* path, std::list<std::string>& dirs, std::list<std::string>& files)
{
	struct dirent* dent;
	struct stat st;
	char fn[FILENAME_MAX] = {0};
	auto len = strlen(path);
	if (len >= FILENAME_MAX - 1) err(EXIT_FAILURE, "Filename too long");
	strcpy(fn, path);
	if (fn[len - 1] != '/')fn[len++] = '/';
	DIR* dir = opendir(path);
	if (!dir) err(EXIT_FAILURE, "Can't open %s", path);
	while ((dent = readdir(dir)))
	{
		if (!strcmp(dent->d_name, ".") || !strcmp(dent->d_name, "..")) continue;
		strncpy(fn + len, dent->d_name, FILENAME_MAX - len);
		if (lstat(fn, &st) == -1)
		{
			warn("Can't stat %s", fn);
			continue;
		}
		auto _fn = std::string(fn);
		if (S_ISDIR(st.st_mode))
			dirs.push_back(
				"<a href=\"" + UrlDecode(_fn.c_str(), _fn.length()) + "/\">" + _fn.substr(len, _fn.length() - len) +
				"/</a><br/>");
		else if (std::regex_match(_fn, std::regex(".+\\.(jpg|png|JPG|PNG)")))
			files.push_back("<li><img src=\"" + UrlDecode(_fn.c_str(), _fn.length()) + "\"/></li>");
	}
	if (dir) closedir(dir);
}

#define HttpHead(value, http, sm) \
	std::regex_search(http, sm, std::regex(value": {0,1}.+?\\r{0,1}\\n"));\
	const auto (value) = std::regex_replace((sm)[0].str(), std::regex("("#value": {0,1}|\\r{0,1}\\n)"), "")

#define HttpUrl(url, http, sm)\
	std::regex_search(http, sm, std::regex("(POST|GET) .+? HTTP"));\
	const auto (url) = std::regex_replace((sm)[0].str(), std::regex("(POST |GET | HTTP|)"), "")

#define HttpHtml(html) \
	("HTTP/1.1 200 OK\r\n\
	Content-length:" + std::to_string((html).length()) + "\r\n\
	Content-Type: text/html; charset=UTF-8\r\n\r\n" + (html))

uint64_t HttpPicture(const char* path, void* buf, uint64_t& size)
{
	const auto pathLen = strlen(path);
	const auto head = "HTTP/1.1 200 OK\r\nContent-length:" +
		std::to_string(size) +
		"\r\nContent-Type: image/" +
		(path[pathLen - 3] == 'p' ? "png" : "jpeg") +
		"\r\n\r\n";
	memcpy(buf, head.c_str(), head.length());
	const auto fp = fopen(path, "rb");
	fread(buf + head.length(), sizeof(uint8_t), size, fp);
	fclose(fp);
	return size + head.length();
}

void IndexOf(const char* path, std::string& response)
{
	auto dirs = std::list<std::string>();
	auto files = std::list<std::string>();
	GetFiles(path, dirs, files);
	const auto dirList = dirs.size()
		                     ? std::accumulate(dirs.begin(), dirs.end(), std::string(""), [&](auto& a, auto& b)
		                     {
			                     return a + b;
		                     })
		                     : " <br/>";
	const auto filesList = files.size()
		                       ? std::accumulate(files.begin(), files.end(), std::string(""), [&](auto& a, auto& b)
		                       {
			                       return a + b;
		                       })
		                       : "";
	const auto html = std::string(" <!DOCTYPE html>") +
		"<html>" +
		"<head><title>Index of " + path + "</title>" +
		R"(<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>)" + 
		R"(<style type="text/css">
img{
	max-width:260px;
	max-height:260px;
}
.ex{
	width:90%;
	background-color: #FFF;
	height: 90%;
	max-width:unset;
	max-height:unset;
}
li{
	width:260px;
	height:260px;
	float:left;
	margin-left:10px;
	margin-top:10px;
	list-style-type:none;
	text-align:center;
}
#TempContainer { 
	position: absolute; 
	z-index: 101; 
	margin-right: 0px; 
	margin-left: 0px; 
	text-align: center; 
	width: 99%; 
	cursor: pointer; 
} 

</style>)" + 
		R"(<script type="text/javascript" src="https://code.jquery.com/jquery-3.4.1.min.js"></script>)" +
		R"(<script type="text/javascript">
$(document).ready(function(e) {
	var ImgsTObj = $('img');
	if(ImgsTObj){ 
		$.each(ImgsTObj,function(){ 
			$(this).click(function(){ 
				var currImg = $(this); 
				var TempContainer = $('<div class=TempContainer></div>'); 
				with(TempContainer){ 
					appendTo("body"); 
					html('<img class="ex" border=0 src="' + currImg.attr('src') + '">'); 
				} 
				TempContainer.click(function(){ 
					$(this).remove(); 
				}); 
			}); 
		}); 
	} 
	else{ 
		return false;
	}
 });
</script>)" +
		"</head>" +
		"<body>" +
		"<h1>Index of " + path + "</h1><hr>" +
		dirList + (dirs.size() ? "<hr>" : "") +
		"<div><ul>" +
		filesList +
		"</ul></div></body>" +
		"</html>";
	response = HttpHtml(html);
}

bool CheckUrl(const std::string url, const char* path)
{
	std::regex_replace(url, std::regex("\\.\\."), "");
	return !memcmp(url.c_str(), path, strlen(path) - 1);
}

void Index(const char* path, const int port)
{
	auto one = 1;
	struct sockaddr_in svrAddr, cliAddr;
	socklen_t sinLen = sizeof(cliAddr);
	auto sock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
	if (sock < 0) err(EXIT_FAILURE, "Can't open socket");
	setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, &one, sizeof(int));
	svrAddr.sin_family = AF_INET;
	svrAddr.sin_addr.s_addr = INADDR_ANY;
	svrAddr.sin_port = htons(port);
	if (bind(sock, (struct sockaddr *)&svrAddr, sizeof(svrAddr)) == -1)
	{
		close(sock);
		err(1, "Can't bind");
	}
	listen(sock, threadNum);
	std::valarray<std::thread> pool(threadNum);
	std::generate(begin(pool), end(pool), [&]()
	{
		return std::thread([&]()
		{
			while (true)
			{
				const auto clientFd = accept(sock, (struct sockaddr *)&cliAddr, &sinLen);
				char buf[1024] = {0};
				auto http = std::string();
				int r = read(clientFd, buf, 1024);
				http.append(buf, r);
				printf("%s:%d===================>\n%s\n", inet_ntoa(cliAddr.sin_addr), ntohs(cliAddr.sin_port),
				       http.c_str());
				std::smatch sm;
				HttpUrl(_url, http, sm);
				auto url = UrlDecode(_url.c_str(), _url.length());
				const auto urlStatus = CheckUrl(url, path);
				if (urlStatus && DirectoryExists(url.c_str()))
				{
					auto response = std::string();
					IndexOf(url.c_str(), response);
					send(clientFd, response.c_str(), response.length(), 0);
				}
				else if (urlStatus && FileExists(url.c_str()))
				{
					auto size = FileSize(url.c_str());
					auto buf = new uint8_t[size + 1024];
					auto bufLen = HttpPicture(url.c_str(), buf, size);
					send(clientFd, buf, bufLen, 0);
					delete[] buf;
				}
				else
				{
					auto response = std::string();
					IndexOf(path, response);
					send(clientFd, response.c_str(), response.length(), 0);
				}
				close(clientFd);
			}
		});
	});
	for (auto& t : pool) t.join();
}

int main(const int argc, char* argv[])
{
	if (argc != 3)
	{
		fprintf(stderr, "%s IndexPath Port\n", argv[0]);
		exit(EXIT_FAILURE);
	}
	Index(argv[1], strtol(argv[2], &argv[2], 10));
}
