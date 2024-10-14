package com.example;

public class Main {
    public static void main(String[] args) {
        int[][] matrix = {
            {1, 0, 0, 0},
            {1, 0, 1, 1},
            {1, 0, 1, 1},
            {0, 1, 0, 0}
        };
        var result = LargestRectangle.safeMaximalRectangle(matrix);
        if (result.errorMessage() == null) {
            System.out.println("Largest rectangle area: " + result.area());
        } else {
            System.out.println(result.errorMessage());
        }
    }
}
