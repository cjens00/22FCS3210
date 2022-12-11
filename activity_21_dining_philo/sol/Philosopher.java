/*
 * CS3210 - Principles of Programming Languages - Fall 2022
 * Instructor: Thyago Mota
 * Description: Activity 21 - Philosopher
 */

import java.util.Random; 

public class Philosopher implements Runnable { 

   private Object leftFork, rightFork; 
   private static final Random rnd = new Random(); 

   Philosopher(Object leftFork, Object rightFork) {
       this.leftFork = leftFork; 
       this.rightFork = rightFork; 
   } 

   void think() { 
       try {
           Thread.sleep(rnd.nextInt(10) + 1); 
       } 
       catch (InterruptedException ex) {} 
   } 
 
   void eat() { 
       try { 
           Thread.sleep(rnd.nextInt(50) + 1); 
       } 
       catch (InterruptedException ex) {} 
   } 

   @Override 
   public void run() { 
       String name = Thread.currentThread().getName(); 
       while (true) { 
           synchronized (leftFork) {
               System.out.println(name + " got the left fork");
               synchronized (rightFork) {
                   System.out.println(name + " got the right fork");
                   System.out.println(name + " is thinking...");
                   think();
                   System.out.println(name + " is eating...");
                   eat();
               }
               System.out.println(name + " released the right fork");
           }
           System.out.println(name + " released the left fork"); 
       } 
   } 
}
