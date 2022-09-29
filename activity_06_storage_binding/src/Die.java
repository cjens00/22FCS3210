/*
 * CS3210 - Principles of Programming Languages - Fall 2022
 * Instructor: Thyago Mota
 * Description: Activity 06 - Storage Binding
 */

import java.util.Random;

/*
A die is defined by a number of sides (at least 2) and a value that keeps the die’s current value (always set to 1 upon initialization). Users should be able to create a dies given its number of sides. If the informed value (as in number of sides) is < 2, set it to 2. Users should also be able to create a die without informing its number of sides (in that case, set the number of sides to 6). Remember, the current value of a die should always be set to 1 upon creation. Create a method called “getValue” that returns the die’s current value. Also, define a method called “roll” to randomly pick another value for the die, based of course on the number of sides.
 */
class Die {

    private int sides;
    private int value;
    private static final int MIN_SIDES = 2;
    private static final int DEFAULT_SIDES = 6;
    private static final int INIT_VALUE = 1;

    public Die(int sides) {
        if (sides < MIN_SIDES)
            this.sides = MIN_SIDES;
        else
            this.sides = sides;
        value = INIT_VALUE;
    }

    public Die() {
        sides = DEFAULT_SIDES;
        value = INIT_VALUE;
    }

    public int getValue() {
        return value;
    }

    // randomly pick another value for the die, based of course on the number of sides.
    public void roll() {
        value = new Random().nextInt(sides) + 1;
    }

    @Override
    public String toString() {
        return "Die{" +
                "sides=" + sides +
                ", value=" + value +
                '}';
    }

    public static void main(String[] args) {
        Die dice[] = { new Die(), new Die(8), new Die(12) };
        for (int i = 0; i < dice.length; i++) {
            dice[i].roll();
            System.out.println(dice[i]);
        }
        Die lucky = new Die(24);
        System.out.println(lucky);
    }
}