package learn;

import java.util.Scanner;
import java.util.Arrays;

public class Main {
  public static void main(String[] args) {
    // Scanner scanner = new Scanner(System.in);
    // String message = scanner.nextLine();
    // Hello.greeting("Your message is: " + message);
    // scanner.close();

    for (int i = 0; i < 0; i++) {
      System.out.println("hi");
    }

    // TicTacToe();
  }

  protected static void TicTacToe() {
    int col;
    int row;
    Scanner input = new Scanner(System.in);

    String[][] board = { { " - ", " - ", " - " },
        { " - ", " - ", " - " },
        { " - ", " - ", " - " } };
    System.out.println("\t" + Arrays.toString(board[0]));
    System.out.println("\t" + Arrays.toString(board[1]));
    System.out.println("\t" + Arrays.toString(board[2]) + "\n");
    System.out.print("X - Select row (0 - 2) & select column (0 - 2) ");
    System.out.print("separated by a space: ");
    row = input.nextInt();
    col = input.nextInt();
    board[row][col] = " X ";
    System.out.println("\t" + Arrays.toString(board[0]));
    System.out.println("\t" + Arrays.toString(board[1]));
    System.out.println("\t" + Arrays.toString(board[2]) + "\n");

  }
}
