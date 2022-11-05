# Instructions

Build a knowledge base (KB) using Prolog to encode the following facts about the highest peaks of each continent and climbers rules preferences: 

```
Mt. Everest is located in Asia and it has 29,029 feet; 
The Aconcagua is located in South America and it has 22,841 feet;  
Mt. McKinley is located in North America and it has 20,312 feet;  
The Kilimanjaro is located in Africa and it has 19,340 feet;  
Mt. Elbrus is located in Europe and it has 18,510 feet;  
Mt. Vinson is located in Antarctica and it has 16,050 feet;  
The Puncak Jaya is located in the Australia (continent) and it has 16,023 feet;  
John, Kelly, Maria, and Derek are certified climbers; 
Thyago is NOT a certified climber;  
John would climb a mountain if he is currently certified and the mountain is located in North America; 
Kelly would climb a mountain if she is currently certified and the mountain has at least 20K feet; 
Maria would climb any mountain, as long as she is currently certified; 
Derek would climb a mountain if he is currently certified and the mountain is located in Europe OR Africa OR Australia, and the mountain has no more than 19K feet; 
Thyago would never climb any of the mountains, not even if he is certified to do it.  
```

To test you KB, you should write Prolog queries to answer the following questions (expected answers are shown):  

```
Is Mt. Everest one of the highest peaks in the world? true 
Is the Kilimanjaro located in Africa? true 
Is Mt. Elbrus more than 18K feet? true 
Is Thyago a certified climber? false 
Is John a certified climber? true 
All certified climbers? John, Kelly, Maria, and Derek 
All peaks that John would climb?  Mt. McKinley 
All peaks that Kelly would climb? Mt. Everest, the Aconcagua, and Mt. McKinley 
All peaks that Maria would climb? all of the 7 peaks! 
All peaks that Derek would climb? Mt. Elbrus and the Puncak Jaya 
All peaks that Thyago would climb? none (or false) 
```

# Submission

Submit a Prolog source file named hwk10.pl (with the KB) using Canvas. Also include all queries that you wrote in Prolog to answer the questions above. 