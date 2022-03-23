#pragma once

#include <filesystem>

#include "Enum.h"
#include "Thread.h"

Enum(LogLevel, None, Error, Warn, Log, Info, Debug)

template<typename T>
class Logger
{
public:
	struct MsgType
	{
		LogLevel Level;
		T Msg;
	};

	LogLevel Level = LogLevel::Debug;
	Thread::Channel<MsgType> Chan{};

	template<LogLevel Lv, typename...Args>
	void Write(Args&&... args)
	{
		if (Lv <= Level)
		{
			WriteImpl<Lv>(T{std::forward<Args>(args)...});
		}
	}

private:
	template<LogLevel Lv>
	void WriteImpl(const T msg)
	{
		Chan.Write(MsgType{ Lv, msg });
	}
};
