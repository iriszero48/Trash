long Ton;
long Toff;
double Vout;
void setup() 
{
	Serial.begin(57600);
	pinMode(2, OUTPUT);
	Ton = 1;
	Toff = 0;
}
void loop() 
{
	digitalWrite(2, HIGH);
	Vout = analogRead(0)*5.0 / 1023.0;
	Serial.print("Vout=");
	Serial.print(Vout);
	Serial.print("V  Ton=");
	Serial.print(Ton);
	Serial.print("  Toff=");
	Serial.println(Toff);
	if (Vout > 2.0)
	{
		if (Ton == 0)
		{
			//++Toff;
		}
		else
		{
			--Ton;
		}
	}
	if (Vout < 2.0)
	{
		if (Toff > 0)
		{
			--Toff;
		}
		else
		{
			++Ton;
		}
	}
	delay(Ton);
	digitalWrite(2, LOW);
	delay(Toff);
}
