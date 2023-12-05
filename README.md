# Course Resources

The **stat528sp24** directory contains all information about our STAT 528 course.

See the Syllabus for more details. See the notes directory for course notes.

# News

 - Welcome to STAT 528!
 - Week 1 reading materials are posted


## Good Habits

 - pull my **stat528sp24** repo before class starts
 - load in datasets and run the code in the notes before class starts
 - make sure that the notes compile 
 - use your computer's terminal, it is less user-friendly but it is MUCH faster
 - Stay centered and remember Hofstadter's Law, which describes the widely experienced difficulty of accurately estimating the time it will take to complete tasks of substantial complexity. **Hofstadter's Law**: It always takes longer than you expect, even when you take into account Hofstadter's Law.
 

## Homework Hints

 - get a jump start on the homework, do not wait until the last minute
 - use caching when appropriate
 - make sure that your assignment compiles when you complete a problem or stop working for the day
 - push often
 - do not jeopardize the entire assignment over one problem


# Course Syllabus

### Greetings 


<img src="https://stat.illinois.edu/sites/default/files/styles/directory_profile/public/profile-photos/dje13.png.jpg?itok=j2rsX2F_" 
     width="150" 
     height="150"
     hspace="25"
     vspace="10"
     align = "left"
     /> I am **Daniel J. Eck** and I will be the instructor for STAT 528: Advanced Regression Analysis II. My research interests include generalized linear regression models which are a major component of this course. I am interested in developing useful statistical methodology for practitioners in a variety of scientific and industrial fields. Several of the examples in this course will come from projects that I have worked on or that I find interesting. 

**Email**: dje13@illinois.edu

<BR CLEAR=”left” /> 
<BR CLEAR=”left” /> 


**Course time:** Tuesdays and Thursdays from 11:00am - 12:20pm
	
**Course location:** 166 Bevier Hall

**Office hours**: 12pm - 1pm on Fridays (Zoom link: TBD)

**TA**: Diptarka Saha (saha12@illinois.edu)

**TA Office hours**: 1pm - 2pm on Fridays (Zoom link: TBD)

**Course website**: 


My course website is a GitHub repository. You can click  [here](https://github.com/illinois-stat528/stat528sp24) to access this website. Note that this Syllabus is a Markdown document. Open the .md file to view the basic Markdown syntax used to build your Syllabus.

**Create your own repository within my course organization**: https://edu.cs.illinois.edu/create-gh-repo/sp24_stat528


***


### Course Materials

The course materials will consist of instructor notes and software. 

#### Instructor Notes

The instructor notes will be posted and updated on GitHub. These notes will reference the following textbooks:  

- **[Categorical Data Analysis](https://www.wiley.com/en-us/Categorical+Data+Analysis%2C+3rd+Edition-p-9780470463635)** by Alan Agresti

- **[Extending the Linear Model with R](https://www.routledge.com/Extending-the-Linear-Model-with-R-Generalized-Linear-Mixed-Effects-and/Faraway/p/book/9781498720960)** by Julian J. Faraway



#### Software Used in the Course (all free)

- **R** https://cran.r-project.org/

- **RStudio** https://www.rstudio.com/products/rstudio/download/

- **RMarkdown** (packaged within RStudio) https://rmarkdown.rstudio.com/lesson-1.html  

- **GitHub** https://web.uillinois.edu/github
  - Details TBD. 

***

### Course Information


This is an advanced introduction to generalized linear models and categorical data analysis with a focus on analyzing data from disciplines such as biostatistics, education, evolutionary biology, medicine, and sports. In this course you will learn how to conduct scientifically relevant data analyses using methodologically rigorous statistical techniques. For example, we have just experienced nearly three semesters of online learning due to COVID-19. Have you wondered about the impact of online learning? In this class we will explore and estimate the causal effects of online learning using causal effect estimators. In addition we are living in a region where soybean is a major crop. As such we will estimate significant gene markers that improve the photosynthetic process in soybeans using mixed-effects models. These analyses will develop your critical thinking skills as a statistician. We will also place a strong emphasis on statistical properties of presented methods. Furthermore, practical advantages, limitations, and comparisons of methods will be discussed. 

**Prerequisites**: [STAT 510](https://courses.illinois.edu/schedule/2021/fall/STAT/510) or [STAT 527](https://courses.illinois.edu/schedule/2021/fall/STAT/527). Familiarity with R, Git, and GitHub is helpful, but not required.


### Student Learning Outcomes

Upon successful completion of this course students will be able to conduct methodologically strong data analyses that can answer questions of scientific interest, specifically the students will: 

  - Analyze data

  - Develop critical thinking about the analysis of data and estimation procedures
 
  - Analyze and investigate fundamentals of exponential family theory
  
  - Develop a solid theoretical and practical understanding of generalized linear models and linear mixed models, generalized linear mixed models, generalized estimation equations, repeated measures designs, and aster models 
  
  - Apply version control software and develop reproducible technical reports


### Getting Help in STAT 528

You have many options to get help for this class:

 * Email me at dje13@illinois.edu from your Illinois email account (@illinois.edu). I will not respond to a non-Illinois email account. Be specific with the topic of your question in the subject line.

 * Canvas - discussion forums, you can post questions.

 * Office hours - you can go to my office hours as described above.
 
 ***

### Grading Breakdown

| category | notes | points
|--|--|--|
| Homework | highest 6 of 7 counted each at 100 points | 600 points 
| Midterm Exam | | 150 points 
| Final Exam | | 250 points
| total | | 1000 points


I will be using a +/- grading schema. The grade distributions will be: 

| Lower bound | Upper bound | Letter grade |
|---|---|---|
| 990 | 1000 | A+ |
| 933 | 989 | A |
| 900 | 932 | A- |
| 867 | 899 | B+ |
| 833 | 866 | B |
| 800 | 832 | B-|
and so on 


#### Homework

There are 7 assignments total and **your lowest assignment score will be dropped**. When completing the assignment, read it carefully, and follow the directions. For each homework assignment, you will submit two files - .Rmd and .html (or .pdf) - saving your files with your name and homework assignment number. **The homework naming convention is netid_HW#**.  For a student with netid *abc123* that is submitting HW4 files, their files would be saved as *abc123_HW4.Rmd* and *abc123_HW4.html* (or *abc123_HW4.pdf*). **Failure to adopt this homework naming convention will result in point deductions and headaches.**  Make sure your homework is neat, readable, and reproducible containing only relevant derivations, code, results, and explanations. You are expected to work on your homework individually since you are graded as an individual. **Questions about the grading should be directed to the TA**. 

Homework should be stored in a HW# directory in your GitHub repo that is a sub directory of a homework directory in your GitHub repo. For example, your fourth homework assignment should be saved in the directory

```
homework/HW4/
```

within your personal GitHub repo.  Failure to do this will result in point deductions.

**Late submissions of the homework will be accepted with a penalty.** There will be a 20 point deduction if an assignment is submitted 48 hours after the deadline. Assignments submitted later than 48 hours after the deadline will not be considered.

**Students must use GitHub to start, finish, and submit their Homework. More details will be discussed in class.**

Homeworks will be worth 100 points unless otherwise noted.


#### Exams
There will be two exams in this class. A midterm examination will be due on Friday, March 10th at 11:59 PM. The final examination will occur at the end of the semester, and it will be due on 05/11 at 11:59 PM. The final examination will be cumulative.

<!-- ### Final Grade Percentage & Letter Grade  -->
<!-- | Lower bound | Upper bound | Letter grade | -->
<!-- |---|---|---| -->
<!-- | 933 | 1000 | A | -->
<!-- | 900 | 932 | A- | -->
<!-- | 867 | 899 | B+ | -->
<!-- | 833 | 866 | B | -->
<!-- | 800 | 832 | B-| -->
<!-- | 767 | 799 | C+ | -->
<!-- | 733 | 766 | C | -->
<!-- | 700 | 732 | C- | -->
<!-- | 667 | 699 | D+ | -->
<!-- | 633 | 666 | D | -->
<!-- | 600 | 632 | D- | -->
<!-- | 0 | 599 | F | -->

<!-- A grade of A+ will be assigned to any student who scores 980 or more points. -->

***


### Policies

#### Disability Accommodations
To obtain disability-related academic adjustments and/or auxiliary aids, students with disabilities must contact the course instructor and the Disability Resources and Educational Services (DRES) as soon as possible. We will try to meet all accommodations once the process has started. Please note accommodations are not retroactive to the beginning of the semester, but begin the day you contact me with a current letter of accommodation from DRES. To contact DRES, you may visit 1207 S. Oak St., Champaign, call 333-4603, e-mail disability@illinois.edu or go to the DRES website at http://disability.illinois.edu/

#### Academic Integrity
It is expected that all students abide by the campus regulations on academic integrity [http://studentcode.illinois.edu/article1_part4_1-401.html].  Intentional violations of academic integrity can be found at http://studentcode.illinois.edu/article1_part4_1-402.html and include, but are not limited to, copying any part of another student's homework, allowing another student to copy any part of your homework, or submitting a review or summary of a presentation not attended.

#### Attendance and Course Content
Students are strongly encouraged to attend class daily. Please follow the Student Code https://studentcode.illinois.edu/docs/18.001.FullCodeInside.vf.pdf if you do have absences. Course content - notes, code, required readings, and data sets - will be found on the Course Website. Do check the Course Website often for updates. Students are encouraged to take notes in class and read materials outside of class. Students are encouraged to save your work in individual GitHub repositories.


#### For Your Safety
We have been asked by Public Safety https://police.illinois.edu/emergency-preparedness/run-hide-fight/ to share the following information in case of weather or security emergencies. See the links:

- [Emergency Response Recommendations](https://police.illinois.edu/emergency-preparedness/run-hide-fight/)
- [Video on Emergency Response](https://mediaspace.illinois.edu/media/t/1_bbti3ec5)

#### Sexual Misconduct Policy and Reporting
The University of Illinois is committed to combating sexual misconduct. Faculty and staff members are required to report any instances of sexual misconduct to the University's Title IX and Disability Office. In turn, an individual with the Title IX and Disability Office will provide information about rights and options, including accommodations, support services, the campus disciplinary process, and law enforcement options. 

A list of the designated University employees who, as counselors, confidential advisors, and medical professionals, do not have this reporting responsibility and can maintain confidentiality, can be found at https://wecare.illinois.edu/resources/students/#confidential. Other information about resources and reporting is available at https://wecare.illinois.edu.


***


### Schedule (subject to change)

* Week 1 [01/16, 01/18]
  + First day of class on 01/16. 
  + Syllabus, course overview 
  + Git/GitHub, R Markdown, statistical programming, and visualization
  + **Reading**: Nelder (1986) and Nelder (1999)

* Week 2 [01/23, 01/25]
  + Git/GitHub, R Markdown, statistical programming, and visualization
  + **Reading**: TBD 
  + Homework 1 due on Friday, 01/26 at 11:59 PM

* Week 3 [01/30, 02/01] 
  + Exponential family theory
  + **Reading**: course notes  

* Week 4 [02/06, 02/08]
  + Exponential family theory
  + **Reading**: course notes
  + Homework 2 due on Friday, 02/09 at 11:59 PM

* Week 5 [02/13, 02/15]
  + Binary regression, data analysis
  + **Reading**: course notes, chapters 5-6, 7.1 of Agresti

* Week 6 [02/20, 02/22]
  + Count regression, data analysis
  + **Reading**: course notes, chapter 5 of Faraway
  + Homework 3 due on Friday, 02/23 at 11:59 PM 
  
* Week 7 [02/27, 02/29]
  + GLM diagnostics
  + **Reading**: course notes

* Week 8 [03/05, 03/07]
  + No class!
  + Office hours during class time
  + Take home midterm exam due on Friday, 03/08 at 11:59 PM

* Week 9    
  + Spring Break
  
* Week 10 [03/19, 03/21]
  + Categorical data analysis
  + **Reading**: course notes, chapter 8 of Agresti, [the VGAM Package for Categorical Data Analysis](https://www.jstatsoft.org/article/view/v032i10)

* Week 11 [03/26, 03/28]
  + Data separation 
  + **Reading**: course notes, sections 6.5, 7.2 of Agresti
  + Homework 4 due on Friday, 03/29 at 11:59 PM  

* Week 12 [04/02, 04/04]
  + LMM
  + **Reading**: course notes, chapters 10-11 of Faraway

* Week 13 [04/09, 04/11]
  + LMM
  + **Reading**: course notes, chapters 10-11 of Faraway
  + Homework 5 due on Friday, 04/12 at 11:59 PM
  
* Week 14 [04/16, 04/18]
  + GLMM/GEE
  + **Reading**: course notes, chapter 13 of Faraway

* Week 15 [04/23, 04/25]
  + Aster models 
  + **Reading**: course notes, materials cited in aster notes
  + Homework 6 due on Friday, 04/26 at 11:59 PM

* Week 16 [04/30]
  + Multivariate regression and envelope basics
  + **Reading**: course notes
  + Last day of instruction on 05/01
  + Final take-home exam assigned
  + Homework 7 due on Wednesday, 05/01 at 11:59 PM

* Final exam due on 05/10 at 11:59 PM.


