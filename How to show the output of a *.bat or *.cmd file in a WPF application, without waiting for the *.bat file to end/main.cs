public void ShowingOutputOfBatFile()
{
    ProcessStartInfo startInfo = new ProcessStartInfo();
    startInfo.FileName = @"C:\Temp\Test.bat";
    startInfo.Arguments = "firstParameter secondParamter";
    startInfo.RedirectStandardError = true;
    startInfo.RedirectStandardOutput = true;
    startInfo.UseShellExecute = false;
    startInfo.WorkingDirectory = @"C:\Temp";
    // Use startInfo.CreateNoWindow = HideWindow; if you want to hide the window
    using (Process process = new Process())
    {
        process.StartInfo = startInfo;
        process.OutputDataReceived += new DataReceivedEventHandler(ProcessOuputHandler);
        process.Start();
        process.BeginOutputReadLine();
        while (!process.HasExited)
        {
            // Refresh you're WPF window here
            myWpfWindow.Dispatcher.Invoke(DispatcherPriority.Render, EmptyDelegate);
            Thread.Sleep(1000);
        }
    }
}
public void ProcessOuputHandler(object sendingProcess, DataReceivedEventArgs outLine)
{
    if (!String.IsNullOrEmpty(outLine.Data))
    {
        if (!outputTextBox.Dispatcher.CheckAccess())
        {
            // Called from a none ui thread, so use dispatcher
            ShowLoggingDelegate showLoggingDelegate = new ShowLoggingDelegate(ShowLogging);
            outputTextBox.Dispatcher.Invoke(DispatcherPriority.Normal, showLoggingDelegate, outLine.Data);
        }
        else
        {
            // Called from UI trhead so just update the textbox
            ShowLogging(outLine.Data);
        };
    }
}
private delegate void ShowLoggingDelegate(string text);
private static Action EmptyDelegate = delegate () { };
/// <summary>
/// Show the logging on screen
/// </summary>
/// <param name="text"></param>
private void ShowLogging(string text)
{
    myWpfWindow.outputTextBox.AppendText(text);
    myWpfWindow.outputTextBox.ScrollToEnd();
}
