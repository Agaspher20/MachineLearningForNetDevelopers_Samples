namespace DigitsRecognizer
{
    using CSharp;
    using System;
    using System.Diagnostics;

    public static class Program
    {
        public static void Main(string[] args)
        {
            Trace.Listeners.Add(new ConsoleTraceListener());
            TestRun.Run();
            Console.ReadKey();
        }
    }
}
