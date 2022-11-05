% Xavier and Brandi are faculty
faculty(xavier).
faculty(brandi).

% Harvey, Ariel, Charlie, and Dan are students 
student(harvey).
student(ariel).
student(charlie).
student(dan).

% Laverne and Ramon are visitors
visitor(laverne).
visitor(ramon).

% Dan is advised by Xavier while Ariel is advised by Brandi
advisor(xavier, dan):- faculty(xavier),student(dan).
advisor(brandi, ariel) :- faculty(brandi),student(ariel).

% days of the week
business(mon). 
business(tue).
business(wed).
business(thu).
business(fri).
weekend(sat).
weekend(sun).

% faculty and students have access to the lab during business days (Monday-Friday); 
% only faculty or students currently being advised by faculty have access to the lab during the weekends.  
access(X, D) :- (faculty(X);student(X)),business(D). 
access(X, business) :- faculty(X);student(X).
access(X, D) :- (faculty(X);advisor(_, X)),weekend(D).
access(X, weekend) :- faculty(X);advisor(_, X).

% Is Xavier a student?  
% student(xavier).

% Is Dan a student?  
% student(dan).

% Is Brandi a faculty?  
% faculty(brandi).

% Is Ariel a visitor?  
% faculty(ariel).

% Does Xavier have access to the lab during business days? 
% access(xavier, business). 

% Does Harvey have access to the lab during business days?  
% access(harvey, business).

% Does Laverne have access to the lab during business days?  
% access(laverne, business).

% Does Xavier have access to the lab during the weekend? 
% access(xavier, weekend).
 
% Does Harvey have access to the lab during the weekend? 
% access(harvey, weekend).
 
% Does Dan have access to the lab during the weekends?  
% access(dan, weekend).

% Does Laverne have access to the lab during the weekends?  
% access(laverne, weekend).
 
% Who is Ariel’s advisor?  
% advisor(X, ariel).

% Who has access to the lab during business days?  
% access(X, business).
%
% Who has access to the lab during the weekends?
% access(X, weekend).
