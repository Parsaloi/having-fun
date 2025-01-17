package com.example;

import net.jqwik.api.*;
import net.jqwik.api.arbitraries.*;
import net.jqwik.api.constraints.*;

import static org.junit.jupiter.api.Assertions.*;

class LargestRectanglePropertyTest {

    @Provide
    Arbitrary<int[][]> matrices() {
        return Arbitraries.integers().between(1, 20).flatMap(rows ->
                Arbitraries.integers().between(1, 20).flatMap(cols ->
                        Arbitraries.integers().between(0, 1).array(int[].class).ofSize(cols)
                                .array(int[][].class).ofSize(rows)));
    }

    @Property
    void resultIsNeverNegative(@ForAll("matrices") int[][] matrix) {
        LargestRectangle.Result result = LargestRectangle.safeMaximalRectangle(matrix);
        assertTrue(result.area() >= 0, "Area should never be negative");
    }

    @Property
    void resultIsNeverLargerThanMatrixSize(@ForAll("matrices") int[][] matrix) {
        LargestRectangle.Result result = LargestRectangle.safeMaximalRectangle(matrix);
        int matrixSize = matrix.length * matrix[0].length;
        assertTrue(result.area() <= matrixSize, "Area should never be larger than the matrix size");
    }

    @Property
    void emptyMatrixReturnsZeroWithoutError() {
        int[][] emptyMatrix = new int[0][0];
        LargestRectangle.Result result = LargestRectangle.safeMaximalRectangle(emptyMatrix);
        assertEquals(0, result.area(), "Empty matrix should return area 0");
        assertNull(result.errorMessage(), "Empty matrix should not produce an error message");
    }

    @Property
    void matrixWithEmptyRowsReturnsZeroWithoutError() {
        int[][] matrixWithEmptyRows = new int[5][0];
        LargestRectangle.Result result = LargestRectangle.safeMaximalRectangle(matrixWithEmptyRows);
        assertEquals(0, result.area(), "Matrix with empty rows should return area 0");
        assertNull(result.errorMessage(), "Matrix with empty rows should not produce an error message");
    }

    @Property
    void matrixWithAllZerosReturnsZero(@ForAll @IntRange(min = 1, max = 20) int rows,
                                       @ForAll @IntRange(min = 1, max = 20) int cols) {
        int[][] zeroMatrix = new int[rows][cols];
        LargestRectangle.Result result = LargestRectangle.safeMaximalRectangle(zeroMatrix);
        assertEquals(0, result.area(), "Matrix with all zeros should return area 0");
        assertNull(result.errorMessage(), "Matrix with all zeros should not produce an error message");
    }

    @Property
    void matrixWithAllOnesReturnsFullArea(@ForAll @IntRange(min = 1, max = 20) int rows,
                                          @ForAll @IntRange(min = 1, max = 20) int cols) {
        int[][] oneMatrix = new int[rows][cols];
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                oneMatrix[i][j] = 1;
            }
        }
        LargestRectangle.Result result = LargestRectangle.safeMaximalRectangle(oneMatrix);
        assertEquals(rows * cols, result.area(), "Matrix with all ones should return full area");
        assertNull(result.errorMessage(), "Matrix with all ones should not produce an error message");
    }

    @Property
    void invalidMatrixReturnsErrorMessage(@ForAll("invalidMatrices") int[][] matrix) {
        LargestRectangle.Result result = LargestRectangle.safeMaximalRectangle(matrix);
        assertEquals(-1, result.area(), "Invalid matrix should return area -1");
        assertNotNull(result.errorMessage(), "Invalid matrix should produce an error message");
    }

    @Provide
    Arbitrary<int[][]> invalidMatrices() {
        return Arbitraries.oneOf(
                Arbitraries.just(null),
                Arbitraries.integers().between(2, 20).map(size -> {
                    int[][] matrix = new int[size][];
                    matrix[0] = new int[size - 1]; // Make the first row shorter
                    for (int i = 1; i < size; i++) {
                        matrix[i] = new int[size];
                    }
                    return matrix;
                }),
                Arbitraries.integers().between(1, 20).flatMap(rows ->
                        Arbitraries.integers().between(1, 20).flatMap(cols ->
                                Arbitraries.integers().between(2, 10).array(int[].class).ofSize(cols)
                                        .array(int[][].class).ofSize(rows)))
        );
    }

    @Property
    void matrixFilledWithZerosReturnsZero(@ForAll @IntRange(min = 1, max = 20) int rows,
                                          @ForAll @IntRange(min = 1, max = 20) int cols) {
        int[][] zeroMatrix = new int[rows][cols];
        LargestRectangle.Result result = LargestRectangle.safeMaximalRectangle(zeroMatrix);
        assertEquals(0, result.area(), "Matrix filled with zeros should return area 0");
        assertNull(result.errorMessage(), "Matrix filled with zeros should not produce an error message");
    }
}
