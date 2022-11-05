% Grace, Omar, Pablo, Molly, and Anthony are students
student(grace).
student(omar).
student(pablo).
student(molly).
student(anthony).

course(che3190).
course(che3200).
course(cs3210).
course(cs3600).
course(bio1080).

% David is a faculty that is enrolled in CHE3190 to help on his research
faculty(david).
enroll(david, che3190) :- (student(david);faculty(david)),course(che3190).

major(cs).
major(che).
major(his).
major(bio).

% Grace and Omar are CS majors
major(grace, cs) :- student(grace),major(cs).
major(omar, cs) :- student(omar),major(cs).

% Pablo is a chemistry major
major(pablo, che) :- student(pablo),major(che).

% Molly is a history major
major(molly, his) :- student(molly),major(his).

% Anthony is a biology major
major(anthony, bio) :- student(anthony),major(bio).

% Grace is enrolled in CS3210 and CS3600
enroll(grace, cs3210) :- (student(grace);faculty(grace)),course(cs3210).
enroll(grace, cs3600) :- (student(grace);faculty(grace)),course(cs3600).

% Omar is enrolled in CS3600
enroll(omar, cs3600) :- (student(omar);faculty(omar)),course(cs3600).

% Pablo is enrolled in CHE3190 and CHE3200
enroll(pablo, che3190) :- (student(pablo);faculty(pablo)),course(che3190).
enroll(pablo, che3200) :- (student(pablo);faculty(pablo)),course(che3200).

% Molly is enrolled in BIO1080
enroll(molly, bio1080) :- (student(molly);faculty(molly)),course(bio1080).

% Anthony is enrolled in BIO1080
enroll(anthony, bio1080) :- (student(anthony);faculty(anthony)),course(bio1080).

% a CS major and is currently taking CS3210; OR  
% currently taking CHE3190 (regardless of their major); OR 
% a biology major
hire(X) :- major(X, cs),enroll(X, cs3210).
hire(X) :- student(X),enroll(X, che3190).
hire(X) :- major(X, bio).


    
