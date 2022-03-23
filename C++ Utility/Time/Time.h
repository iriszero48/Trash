#pragma once

#include <ctime>

namespace Time
{
    constexpr int Version[] { 1, 0, 0, 0 };

    inline void Gmt(tm* gmt, time_t* time)
	{
#if (defined _WIN32 || _WIN64)
		gmtime_s(gmt, time);
#else
		gmtime_r(time, gmt);
#endif
	}

    inline void Local(tm* local, time_t* time)
	{
#if (defined _WIN32 || _WIN64)
		localtime_s(local, time);
#else
		localtime_r(time, local);
#endif
	}
}
