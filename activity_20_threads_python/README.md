Threads in Python are supported via the threading module. The simplest way to use a Thread  object is to instantiate it with a target parameter and call start() to let it begin working. 

```
import threading 
import time 

def run(): 
    name = threading.currentThread().getName() 
    for i in range(5): 
        time.sleep(1) 
        print(name + ': ' + str(i)) 

if __name__ == "__main__": 
 
    t1 = threading.Thread(target = run) 
    t1.setName('t1') 
 
    t2 = threading.Thread(target = run) 
    t2.setName('t2') 

    t1.start() 
    t2.start() 
```

If your thread needs input parameters you can pass the args parameter to the Thread’s constructors, as shown in the example below.  

```
import threading 
import time 

def run(name): 
    for i in range(5): 
        time.sleep(1) 
        print(name + ': ' + str(i)) 

if __name__ == "__main__": 

    t1 = threading.Thread(target = run, args = ('t1',)) 
    t2 = threading.Thread(target = run, args = ('t2',)) 

    t1.start() 
    t2.start() 
```

Alternatively, you can create your own thread class by specializing it from Thread. In that case, you should override the Thread’s class run method to define your thread’s line of execution. The example below illustrates that.  

```
import threading 
import time 

class MyThread(threading.Thread):  

    def __init__(self, name): 
        threading.Thread.__init__(self) 
        self.name = name 

    def run(self): 
        for i in range(5): 
            time.sleep(1) 
            print(self.name + ': ' + str(i)) 
 
if __name__ == "__main__": 
 
    t1 = MyThread('t1') 
    t2 = MyThread('t2') 

    t1.start() 
    t2.start() 
```

Now that you understand how to define threads in Python, run the following example that illustrates a race condition situation. Run the code a couple of times. You should be able to get different results, like 100 and -100, for example. Talk to a classmate next to you and discuss what you think is causing the race condition.  

```
import threading 
import time 

class Counter: 

    def __init__(self): 
        self.c = 0 

    def getValue(self): 
        return self.c 

    def setValue(self, newC): 
        self.c = newC 

class IncrementThread(threading.Thread): 
 
    def __init__(self, counter): 
        threading.Thread.__init__(self) 
        self.counter = counter 
 
    def run(self): 
        for i in range(100): 
            c = self.counter.getValue() 
            c = c + 1 
            time.sleep(0.01) 
            self.counter.setValue(c) 
 
class DecrementThread(threading.Thread): 
 
    def __init__(self, counter): 
        threading.Thread.__init__(self) 
        self.counter = counter 
 
    def run(self): 
        for i in range(100): 
            c = self.counter.getValue() 
            c = c - 1 
            time.sleep(0.01) 
            self.counter.setValue(c) 

if __name__ == "__main__": 
 
    counter = Counter() 
 
    t1 = IncrementThread(counter) 
    t2 = DecrementThread(counter) 

    t1.start() 
    t2.start() 

    t1.join() 
    t2.join() 

    print('Counter value is ' + str(counter.getValue())) 
```

Change the implementation to avoid the race condition problem.   