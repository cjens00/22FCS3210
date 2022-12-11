There are two ways to define a thread in Java.  One is by specializing the Thread class and overriding its run method to implement the thread’s line of execution.  The example below illustrates how to define threads this way.  

```
class MyThread extends Thread { 
   @Override 
   public void run() { 
       String name = Thread.currentThread().getName(); 
       for (int i = 0; i < 5; i++) { 
           try { 
               Thread.sleep(1000); 
           } catch (InterruptedException e) { 
               e.printStackTrace(); 
           } 
           System.out.println(name + ": " + i); 
       } 
   } 
} 

class HelloJavaThreadsv1 { 

   public static void main(String[] args) {
       Thread t1 = new MyThread(); 
       t1.setName("t1"); 
 
       Thread t2 = new MyThread(); 
       t2.setName("t2"); 

       // the start method call begins the execution of a thread 
       t1.start();  
       t2.start(); 
   } 
} 
```

Sometimes you cannot define threads this way because your class is already a specialization of another class.  In this case, you can define your thread by having your class implement the Runnable interface and (again) overriding its run method. The example below illustrates how to define threads this way.  

```
public class HelloJavaThreadsv2 { 

   public static void main(String[] args) { 
       Runnable runnable = new Runnable() { 
           @Override 
           public void run() { 
               String name = Thread.currentThread().getName(); 
               for (int i = 0; i < 5; i++) { 
                   try { 
                       Thread.sleep(1000); 
                   } catch (InterruptedException e) { 
                       e.printStackTrace(); 
                   } 
                   System.out.println(name + ": " + i); 
               } 
           } 
       }; 
 
       Thread t1 = new Thread(runnable); 
       t1.setName("t1"); 
 
       Thread t2 = new Thread(runnable); 
       t2.setName("t2"); 
 
       t1.start(); 
       t2.start(); 
   } 
} 
```

Now that you understand how to define threads in Java, run the following example that illustrates a race condition situation.  

```
class Counter { 
   private int c = 0; 

   void increment() { 
       c++; 
   } 

   void decrement() { 
       c--; 
   } 

   int value() { 
       return c; 
   } 
} 

class IncrementThread extends Thread { 

   private Counter counter; 
 
   public IncrementThread(Counter counter) { 
       this.counter = counter; 
   } 

   @Override 
   public void run() { 
       String name = Thread.currentThread().getName(); 
       for (int i = 0; i < 100; i++) { 
           try { 
               Thread.sleep(10); 
           } catch (InterruptedException e) { 
               e.printStackTrace(); 
           } 
           counter.increment(); 
       } 
   } 
} 

class DecrementThread extends Thread { 

   private Counter counter;  

   public DecrementThread(Counter counter) { 
       this.counter = counter; 
   } 

   @Override 
   public void run() { 
       String name = Thread.currentThread().getName(); 
       for (int i = 0; i < 100; i++) { 
           try { 
               Thread.sleep(10); 
           } catch (InterruptedException e) { 
               e.printStackTrace(); 
           } 
           counter.decrement(); 
       } 
   } 
} 

class RaceConditionExample { 

   public static void main(String[] args) { 
 
       Counter counter = new Counter(); 

       Thread incThread = new IncrementThread(counter); 
       Thread decThread = new DecrementThread(counter); 

       incThread.start(); 
       decThread.start(); 

       // wait for both threads to finish 
       try { 
           incThread.join(); 
           decThread.join(); 
       } catch (InterruptedException e) { 
           e.printStackTrace(); 
       } 
 
       System.out.println("Counter value is " + counter.value()); 
   } 
} 
```

Change the implementation to avoid the race condition problem.   