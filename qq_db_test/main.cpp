#include <Windows.h>
#include <iostream>

using sqlite3 = int;
using sqlite3_open = int (*)(const char *filename, sqlite3 **ppDb);
using sqlite3_key = int (*)(sqlite3 *db, const void *pKey, int nKey);
using sqlite3_rekey = int (*)(sqlite3 *db, const void *pKey, int nKey);
using sqlite3_exec = int (*)(sqlite3 *db, const char *sql, int (*callback)(void *, int, char **, char **), void *, char **errmsg);

int main()
{
	constexpr auto db_path = R"(E:\tmp\Msg3.0.db)";
	const char *key = "xxxxxxxxxxxxxx";

	auto dll = LoadLibraryEx(LR"(D:\Program Files (x86)\Tencent\QQ\Bin\KernelUtil.dll)", NULL, LOAD_WITH_ALTERED_SEARCH_PATH);
	if (dll == NULL)
	{
		std::cout << "null";
	}

	auto do_open = (sqlite3_open)((DWORD)dll + 222630);
	auto do_key = (sqlite3_key)((DWORD)dll + 552582);
	auto do_rekey = (sqlite3_rekey)((DWORD)dll + 553029);
	auto do_exec = (sqlite3_exec)((DWORD)dll + 219495);

	sqlite3 *db = NULL;

	auto ret = do_open(db_path, &db);
	std::cout << "ret " << ret << std::endl;

	ret = do_key(db, key, 16);
	std::cout << "ret " << ret << std::endl;

	// ret = do_rekey(db, "", 0);
	// std::cout << "ret " << ret << std::endl;

	char *err{};
	ret = do_exec(
		db, "select * from sqlite_master", [](void *param, int col_count, char **values, char **cols) -> int
		{
			for (size_t i = 0; i < col_count; ++i)
			{
				std::cout << cols[i] << ": " << values[i];
				if (i != col_count - 1)
				{
					std::cout << ", ";
				}
				else
				{
					std::cout << std::endl;
				}
			}

			return 0; },
		NULL, &err);
	std::cout << "err " << std::string(err) << std::endl;
	std::cout << "ret " << ret << std::endl;
}