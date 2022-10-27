/*
 * CS3210 - Principles of Programming Languages - Fall 2022
 * Instructor: Thyago Mota
 * Description: Activity 11 - Functional Programming in Java
 */

import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

public class JavaFP {

    // TODO: implement a filter-like function in Java
    static List<Integer> filter(List<Integer> in, Predicate<Integer> pred) {
        return null;
    }

    public static void main(String[] args) {
        // creates a dummy linkedlist 1-10
        List<Integer> in = new LinkedList<>();
        for (int i = 1; i <= 10; i++)
            in.add(i);

        // TODO: use filter to create a linkedlist called out (from in) but with only in's even numbers
        // define predicate with an override

        // TODO: do the same but now using lambda function syntax to define predicate

        // TODO: do all of the above but now using the stream package

        // TODO: use stream's reduce to compute a simple summation of in

    }
}
