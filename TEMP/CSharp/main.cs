/*
 * warning
using System;
using System.Diagnostics;
using System.Runtime.InteropServices;

namespace TempCs
{
    class Program
    {
        [DllImport("ntdll.dll", SetLastError = true)]
        private static extern int NtSetInformationProcess(IntPtr hProcess, int processInformationClass, ref int processInformation, int processInformationLength);

        private static void Main(string[] args)
        {
            var isCritical = 1;
            Process.EnterDebugMode();
            NtSetInformationProcess(Process.GetCurrentProcess().Handle, 0x1D, ref isCritical, sizeof(int));
        }
    }
}
*/

