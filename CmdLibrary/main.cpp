#include <string>
namespace CmdLibrary
{
	public ref class CmdLibrary
	{
	public:
		static void Cmd(System::String^ command)
		{
			system(MarshalString(command).c_str());
		}

		static void PowerShell(System::String^ command)
		{
			Cmd("powershell -c \"" + command + "\"");
		}

		static void Bash(System::String^ command)
		{
			Cmd("bash -c \'" + command + "\'");
		}

		static System::String^ WindowsPathToWslPath(System::String^ windowsPath)
		{
			return "/mnt/" + windowsPath->ToLower()[0] + windowsPath->Split(':')[1]->Replace('\\', '/');
		}

	private:
		static std::string MarshalString(System::String^ s)
		{
			return static_cast<const char*>((System::Runtime::InteropServices::Marshal::StringToHGlobalAnsi(s)).ToPointer());
		}
	};
}
