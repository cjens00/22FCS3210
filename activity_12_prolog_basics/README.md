# Example 1

Consider a college scenario where faculty, students, and visitors have controlled access to a lab during week and weekend days to encode the following facts in Prolog:  

```
Xavier and Brandi are faculty
Harvey, Ariel, Charlie, and Dan are students 
Laverne and Ramon are visitors
Dan is advised by Xavier while Ariel is advised by Brandi
```

Next, encode the following rules: 

```
faculty and students have access to the lab during business days (Monday-Friday); 
only faculty or students currently being advised by faculty have access to the lab during the weekends.  
```
 
Finally, have Prolog answer the following queries:  

```
Is Xavier a student?  
Is Dan a student?  
Is Brandi a faculty?  
Is Ariel a visitor?  
Does Xavier have access to the lab during business days?  
Does Harvey have access to the lab during business days?  
Does Laverne have access to the lab during business days?  
Does Xavier have access to the lab during the weekend?  
Does Harvey have access to the lab during the weekend?  
Does Dan have access to the lab during the weekends?  
Does Laverne have access to the lab during the weekends?   
Who is Ariel’s advisor?  
Who has access to the lab during business days?  
Who has access to the lab during the weekends?  
```

# Example 2

Using Prolog, encode the following facts about students enrolled in classes:  

```
Grace, Omar, Pablo, Molly, and Anthony are students
David is a faculty that is enrolled in CHE3190 to help on his research
Grace and Omar are CS majors
Pablo is a chemistry major
Molly is a history major
Anthony is a biology major
Grace is enrolled in CS3210 and CS3600
Omar is enrolled in CS3600
Pablo is enrolled in CHE3190 and CHE3200
Molly is enrolled in BIO1080
Anthony is enrolled in BIO1080
```

A biotech company is willing to hire student interns for the summer IF the student is:  

```
a CS major and is currently taking CS3210; OR  
currently taking CHE3190 (regardless of their major); OR 
a biology major
```

Have Prolog answer which student(s) should be hired.   