
# Introduction to Computational Resources

## Welcome

Welcome to STAT 528, let's have a productive semester!

This document will go through GitHub, computational resources, software, and overall course organization. This will be a valuable resource for you as you become familiar with this course's GitHub based organization in which every student will submit assignments by pushing to an individual GitHub repository that resides within my course's GitHub organization.

You should have downloaded this document from the course website https://github.com/illinois-stat528/stat528sp24 or some other source. 


### Reproducible Documentation

Here I mean that we will use good principles and software (R and RStudio) that allow anyone (with similar software) to be able to look at our input (.Rmd) and the code therein and re-create the same output (.html or .pdf) and come to the same conclusion that we did. To do this, we need to keep good notes in our code documents and version control can aid us.  We will need to set R's random seed for every document that uses random number generation, so that the results are reproducible.

Creating reproducible experiments is necessary for all scientific progress. Results that cannot be verified by a third-party should be viewed with skepticism. Data analyses and numerical simulations are no exception.

Computer based assignments will be written in R Markdown. In case you were wondering, this is an R Markdown document. Hyperlinks like the one in this sentence can be [embedded in the text](http://rmarkdown.rstudio.com) using Markdown. Read the corresponding .Rmd file to see how this is done. [This reference contains a lot of useful information on Markdown syntax that will help you in this class](https://www.markdownguide.org/basic-syntax/).


### Version Control 

In this course, we will use Git and GitHub Enterprise. GitHub Enterprise is a Github service that we have thanks to our good friends in the Computer Science Department. Git is one of several version control software. It allows users to save a document with time stamps and comments on what was changed in that document systematically. We will discuss much more about this later in this document.


### Work Flow

You will use Markdown and GitHub to submit labs, homeworks, projects, etc. You will obtain your homework assignment from my GitHub course page (my **stat528sp24** repository) and will submit them to your individual GitHub repository. Eventually this will all make sense and you will be comfortable with it!


## Software needed for this class

We will be working with [R](https://cran.r-project.org) and [RStudio](https://rstudio.com). 

R is a statistical programming language. It is free and open-source and is being developed by users to handle tasks beyond statistics and data science. We code in R, usually in a **script** and saved as an .R file or .Rmd file. The output of the code we run is visible in the **console**.

RStudio is an interface for R that is relatively user-friendly, visually sufficient, and well-organized for content management. In RStudio, we will do almost everything we need for this course - write R Markdown files (.Rmd), knit the .Rmd to .html, and repeatedly interact with GitHub repositories.


### R and RStudio Tasks To Do

1. Install and update R to the latest version.

2. Install and update RStudio to the latest version.

3. Create a new R Markdown file using the Wizard in RStudio.

4. Create a pdf or html file by knitting the R Markdown file.

5. Locate the file in our directory.

6. Open a pdf or html file.


## Create your individual GitHub repositories

Repositories (**repos** for short) are essentially project folders where you intend to store a set of files in the same location, much like a folder. These repositories are constantly being updated and are in need of version of control. This is where GitHub and other Git platforms come in. GitHub allows you to make your edits to a project locally and save them remotely while keeping all previous versions to your project intact. You do not need to constantly change file names to indicate new versions, just push your edits to GitHub. GitHub will store the previous versions and a detailed record of the changes made. GitHub and other version control platforms are fundamentally important for collaboration.

We will now start the setup of your individual student **GitHub repositories**. Each student should click on this link https://edu.cs.illinois.edu/create-gh-repo/sp24_stat528 in order to create your student repo. Your personal repo is where you will keep your assignment files. The idea is that:  
- you will work on your assignments from your repo, updating it often to ensure you're working on the correct file(s)  
- you will obtain course resources, including homework assignments, from my **stat528sp24** course repo.  

GitHub will be a reocurring topic throughout this course.


### Git and GitHub Tasks to Do

1. Setup your individual repos if you have not yet done so. 

Your repo has a very special address which you need to **clone** onto your local computer. Cloning the repo connects your GitHub repo to your local computer as a directory (with the same name as the repo). After we access Git via a terminal or shell, we will clone our repos. Make sure that you know which directory that you clone your repo in! I recommend creating a **stat528** directory on your Desktop and then cloning your repo in this directory.

3. Open your computer's terminal or use RStudio to open the Terminal. To do the later, click "Tools", then "Terminal", then "New Terminal." This Terminal is a shell that allows us to access Git via the command-line. 

4. Go to my  **stat528sp24** repo in GitHub (on your browser) and clone my repo by clicking the "Clone or Download" button.

5. Open your terminal (shell) and navigate your terminal to your **stat528** course directory using **cd**. 

6. When your terminal's working directory is **stat528** then you are ready to clone my repo. First clone my repo by pasting the contents that were copied from clicking the "Clone or Download" button to a terminal command starting with:

```
git clone
```

You can also clone my repo by directly typing the following into your terminal:

```
git clone git@github.com:illinois-stat528/stat528sp24.git
```

When you type the above you may get the following error message:

```
ERROR: The `illinois-stat528' organization has enabled or enforced SAML SSO. To access
this repository, you must use the HTTPS remote with a personal access token
or SSH with an SSH key and passphrase
that has been authorized for this organization. Visit
https://docs.github.com/articles/authenticating-to-a-github-organization-with-saml-single-sign-on/ for more information.

fatal: Could not read from remote repository.

Please make sure you have the correct access rights
and the repository exists.
```

If you do get this error message, go to [this link](https://docs.github.com/articles/authenticating-to-a-github-organization-with-saml-single-sign-on/) and learn about SAML SSO. You may need to generate an [SSH key](https://docs.github.com/en/authentication/connecting-to-github-with-ssh/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent), [add your SSH key to your GitHub account](https://docs.github.com/en/enterprise-server@3.0/authentication/connecting-to-github-with-ssh/adding-a-new-ssh-key-to-your-github-account), and then [configure SSO](https://docs.github.com/en/enterprise-cloud@latest/authentication/authenticating-with-saml-single-sign-on/authorizing-an-ssh-key-for-use-with-saml-single-sign-on) for this course's GitHub organization (it may say Configure SSO instead of Enable SSO in your SSH and GPG keys GitHub page).


**Do not edit any files in your local versions of my stat528sp24 repo**. The **stat528sp24** repo is an internal repo that you can only pull materials from, you do not have writing permissions in this repo. Editing files in the **stat528sp24** repo  will create a [conflict](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/about-merge-conflicts). You should copy files from my **stat528sp24** repo to your desired destination.



Now clone your personal repo into your **stat528**  directory by following the same steps that cloned my repo into this directory. You can do this by first changing *YourNetID.git* to your actual netid .git in the following and then coping the command into the terminal.

```
git clone git@github.com:illinois-stat528/sp24_stat528_YourNetID.git
```

These commands establish the connection between GitHub (remote) and your computer (local). **You only need to clone your repos once, make sure that your repos are inside your stat528 directory and are not nested within one another.** You can always delete either of these repos and reclone if you need to.


7. (Optional) Using the terminal or the shell,  

- Make your repo your current working directory using **cd**

- List its files  
```
ls
```

- Display the README

```
head README.md
```

- Get some information on its connection to GitHub
```
git remote show origin
```

These are simple commands that we use to work in Git. If done correctly, the message that appears below the `git remote show origin` tells us that the connection to GitHub was successful.

**Cloning your repo should happen only once per computer** (but we will repeat this in RStudio later). Meaning we should never have to re-establish the connection to GitHub. 

8. Using the shell, edit the README.md file and verify Git status.  

```
echo "The second thing I've written but now in Git from the command-line" >> README.md
git status
```

9. Using the shell: stage the change, commit it with a message, and push to your GitHub repo. Notice that you must describe the update that you are making. This action is called a **commit.** The commit is the message that you are leaving yourself to note what/why/how this file has been changed. Commits can be short (Git prefers this), but could be long. As you make future changes to this file or other files, you're going to want to commit for every change (or at least the noticeable changes). The following three lines stages, commits, and pushes content from your local computer to your remote repository:
```
git add -A
git commit -m "Added new sentence from local computer"
git push 
```
The -A flag in the first line of the above code adds all changed values to your commit. You can add individual files if you choose to do so. The latter method can be preferable in many cases. In this example, the latter method is achieved by typing:
```
git add README.md
git commit -m "Added new sentence from local computer"
git push 
```
Go online to view your changes.


10. Using GitHub, verify the new change by refreshing the GitHub repo page.

**Important:** These tasks are needed in order to complete the first lab assignment.
