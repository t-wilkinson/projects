#include "IRremote.h"

/*-----( Global Constants )-----*/

/*-----( Declare objects )-----*/
IRrecv irrecv(11);     // create instance of 'irrecv'
IRData *data;      // create instance of 'decode_results'

/*-----( Function )-----*/
void translateIR() {          // takes action based on IR code received
// describing Remote IR codes

  switch(data->command){
    case 69: Serial.println("POWER"); break;
    case 0xFFE21D: Serial.println("FUNC/STOP"); break;
    case 0xFF629D: Serial.println("VOL+"); break;
    case 0xFF22DD: Serial.println("FAST BACK");    break;
    case 0xFF02FD: Serial.println("PAUSE");    break;
    case 0xFFC23D: Serial.println("FAST FORWARD");   break;
    case 0xFFE01F: Serial.println("DOWN");    break;
    case 0xFFA857: Serial.println("VOL-");    break;
    case 0xFF906F: Serial.println("UP");    break;
    case 0xFF9867: Serial.println("EQ");    break;
    case 0xFFB04F: Serial.println("ST/REPT");    break;
    case 22: Serial.println("0");    break;
    case 12: Serial.println("1");    break;
    case 24: Serial.println("2");    break;
    case 94: Serial.println("3");    break;
    case 8: Serial.println("4");    break;
    case 28: Serial.println("5");    break;
    case 90: Serial.println("6");    break;
    case 66: Serial.println("7");    break;
    case 82: Serial.println("8");    break;
    case 74: Serial.println("9");    break;
    case 0xFFFFFFFF: Serial.println(" REPEAT");break;

  default:
    Serial.print(" other button   ");
    Serial.println(data->command);

  }// End Case

} //END translateIR

void setup(){   /*----( SETUP: RUNS ONCE )----*/
  pinMode(12, OUTPUT);
  Serial.begin(1200);
  Serial.println("IR Receiver Button Decode");
  irrecv.start();           // Start the receiver

}/*--(end setup )---*/

void toggleButton() {
    static int ledOn = LOW;
    if (data->command == 69 && ledOn == LOW) {
      ledOn = HIGH;
    } else if (data->command == 69 && ledOn == HIGH) {
      ledOn = LOW;
    }
    digitalWrite(12, ledOn);
}

void loop(){   /*----( LOOP: RUNS CONSTANTLY )----*/
  if (irrecv.decode()) {
    data = irrecv.read();
    toggleButton();

    translateIR();
    delay(500);
    irrecv.resume();
  }
}/* --(end main loop )-- */
