using System;
using static System.Console;
using System.Text;

namespace TEMP_CS
{
    public class Program
    {
        public static void Main(string[] args)
        {
            Write(Encoding.Default.GetString(Base32.FromBase32String(args[0])));
        }
    }
    internal sealed class Base32
    {
        private const int InByteSize = 8;
        private const int OutByteSize = 5;
        private const string Base32Alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567";
        internal static byte[] FromBase32String(string base32String)
        {
            if (base32String == null)
            {
                return null;
            }
            else if (base32String == string.Empty)
            {
                return new byte[0];
            }
            string base32StringUpperCase = base32String.ToUpperInvariant();
            byte[] outputBytes = new byte[base32StringUpperCase.Length * OutByteSize / InByteSize];
            if (outputBytes.Length == 0)
            {
                throw new ArgumentException("Specified string is not valid Base32 format because it doesn't have enough data to construct a complete byte array");
            }
            int base32Position = 0;
            int base32SubPosition = 0;
            int outputBytePosition = 0;
            int outputByteSubPosition = 0;
            while (outputBytePosition < outputBytes.Length)
            {
                int currentBase32Byte = Base32Alphabet.IndexOf(base32StringUpperCase[base32Position]);
                if (currentBase32Byte < 0)
                {
                    throw new ArgumentException(string.Format("Specified string is not valid Base32 format because character \"{0}\" does not exist in Base32 alphabet", base32String[base32Position]));
                }
                int bitsAvailableInByte = Math.Min(OutByteSize - base32SubPosition, InByteSize - outputByteSubPosition);
                outputBytes[outputBytePosition] <<= bitsAvailableInByte;
                outputBytes[outputBytePosition] |= (byte)(currentBase32Byte >> (OutByteSize - (base32SubPosition + bitsAvailableInByte)));
                outputByteSubPosition += bitsAvailableInByte;
                if (outputByteSubPosition >= InByteSize)
                {
                    outputBytePosition++;
                    outputByteSubPosition = 0;
                }
                base32SubPosition += bitsAvailableInByte;
                if (base32SubPosition >= OutByteSize)
                {
                    base32Position++;
                    base32SubPosition = 0;
                }
            }
            return outputBytes;
        }
    }
}
