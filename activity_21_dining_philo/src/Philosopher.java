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
        
   } 

   void think() { 
       try { 
           
       } 
       catch (InterruptedException ex) {} 
   } 
 
   void eat() { 
       try { 
            
       } 
       catch (InterruptedException ex) {} 
   } 

   @Override 
   public void run() { 
       String name = Thread.currentThread().getName(); 
       while (true) { 
           
       } 
   } 
}
