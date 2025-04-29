package learn;

import java.util.Scanner;

public class Main {
  public static void main(String[] args) {
    Scanner scanner = new Scanner(System.in);
    String message = scanner.nextLine();
    Hello.greeting("Your message is: " + message);
    scanner.close();
  }
}
