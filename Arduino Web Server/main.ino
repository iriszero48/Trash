#include <EtherCard.h>
static byte mymac[] = { 0x74,0x69,0x69,0x2D,0x30,0x31 };
static byte myip[] = { 192,168,3,69 };
byte Ethernet::buffer[1500];
BufferFiller bfill;
void setup() 
{
	Serial.begin(57600);
	Serial.println("Cardinal");
	if (ether.begin(sizeof Ethernet::buffer, mymac) == 0)
	{
		Serial.println(F("Failed to access Ethernet controller"));
	}
	ether.staticSetup(myip);
}
static word homePage() 
{
	long t = millis() / 1000;
	word h = t / 3600;
	byte m = (t / 60) % 60;
	byte s = t % 60;
	bfill = ether.tcpOffset();
	bfill.emit_p(PSTR(
		"HTTP/1.0 200 OK\r\n"
		"Content-Type: text/html\r\n"
		"Pragma: no-cache\r\n"
		"\r\n"
		"<meta http-equiv='refresh' content='1'/><title>Cardinal</title><style type='text/css'>body{background-color: #3399cc}h1{opacity:0.8;color:#fff;font-family:'Source Sans Pro',sans-serif;text-align:center;}a{text-decoration:none}a:link,a:visited{color:#fff}</style><h1>$D$D:$D$D:$D$D</h1>"
	),
		h / 10, h % 10, m / 10, m % 10, s / 10, s % 10);
	return bfill.position();
}

void loop() 
{
	word len = ether.packetReceive();
	word pos = ether.packetLoop(len);
	Serial.println("Cardinal");
	if (pos)
	{
		ether.httpServerReply(homePage());
	}
}
