## Step 1: Brainstorming 

This part of the 'Course Spec' process is designed to help guide you through course design by having you think through several key questions. Please make sure to delete the examples provided here for you.

### A. What problem(s) will students learn how to solve? (minimum of 5 problems)

*Consider technical problems and 'use case' or business problems.*

- [ ] Write a list of ideas for problems that the students will encounter in the course.

- How to create a simple R package including a data set and an R Markdown template file
- How to expand on this package to include additional functions
- How to decipher the structure of an R package including required and optional files and and also for directories
- How to have an efficient workflow for developing functions in the package
- How to map out package vignettes to show helpful documentation and use cases
- How to make unit tests for your package's functions

> From a course on fraud detection:
>
>- How to identify and predict fraudulent transactions
>- How to effectively work with highly imbalanced data


### B. What are the learning objectives of the course?

- [ ] Write a list of learning objectives for the course. These are not shown to the students, but they will be used to ensure your vision for the course aligns with the vision of your Curriculum Manager.

- Learn how to create the package structure for a package from R
- Learn how to make an R Markdown template file
- Learn how to include data in your package
- Learn how to check to see if your package template can be used
- Learn how to explain the components of a DESCRIPTION file
- Learn how to explain the components of a NAMESPACE file
- Learn how to describe the structure of the `R` directory
- Learn how to describe the structure of the `man` directory
- Learn what the optional R package directories are and how they are used
- Refresh your knowledge of writing R functions

- Learn how to structure code and functions in the `R` directory
- Learn best paractices on naming your package
- Learn how to see if your package name is available on CRAN
- Learn how to identify the different license options available for sharing your package
- Learn how to load all files in your package to begin testing your package functionality
- Learn how to check that your package meets the requirements of R packages programmatically
- Learn how to install your package locally
- Learn how to add CRAN packages as dependencies to your package
- Learn the different types of dependencies

- Learn how to decipher the components of a help file with roxygen header documentation for a particular function
- Learn the differences between exported and non-exported functions
- Learn how to set a function as exported
- Learn how to create examples in roxygen
- Learn how to document a package and the smallest amount of detail needed to do so
- Learn how to document data
- Learn how to create documentation files
- Learn what an R package vignette is
- Learn how to look over vignettes for a particular installed package
- Learn how to create a vignette in the `vignettes` directory
- Learn how to add necessary vignette dependencies to `DESCRIPTION`
- Learn the best practices on building the vignette
- Learn how to read the vignette's metadata
- Learn the difference between a vignette and an article
- Learn how to build vignettes individually and all at once

- Learn what a unit test is and why they are important to package development and maintenance
- Learn how to create a test file in the appropriate path
- Learn what the most often used expect statements are in testing
- Learn how to check if two R objects are the same in value, attribute, and type 
- Learn how to test for R function calls producing specific or non-specific errors
- Learn how to test for R function calls producing specific or non-specific messages
- Learn how to test output of an R function matching the given output
- Learn how to organize tests into a single `testthat` function call
- ~Learn about what is meant by a testing context~ `context()` is no longer recommended
- Learn how to run unit tests and how to decipher the output

- Learn how to share your package on GitHub
- Learn how to use continuous integration with your package

>Example from our scikit-learn course:
>
>- Learn the key concepts of supervised learning and how to implement them on real-world datasets;
>- Learn to distinguish regression from classification problems;
>- Learn to evaluate how well your classification and regression modes perform;
>- Learn best practices in supervised learning, such as splitting into test/train sets and k-fold cross validation;
>- Learn how to improve model performance by both preprocessing your data and regularizing your models.


### C. What technologies, packages, or functions will students use? Please be exhaustive.

- [ ] Write a list of ideas for technologies that you want to use in the course. Include things like R/Python packages, SQL modules, or Google Sheets add-ons. If there are any important functions, methods, or commands that you want to teach, you can mention them here.

R will be the technology. The packages used in the course will largely be `devtools`, `usethis`, `roxygen2`, and `testthat`.

Functions students will use are listed in the outline below.

>Example from a course on keras. This example has only a few Python packages and goes into depth on the functions that will be used.
>
>- keras, pandas, sklearn
>- Keras functions:
>- Dense
>- Concatenate, Subtract, Multiply (operate on two layers)
>- l2
>- Embedding
>- Flatten
>- keras.preprocessing.text.text_to_word_sequence
>- keras.preprocessing.sequence.pad_sequences
>- GRU
>- Bidirectional

- [ ] Add the packages students will use to the requirements.r file of this repository.


### D. What terms or jargon will you define?

- [ ] Write a list of technical terms, jargon, and acronyms that will be used in the course and define them as well.

> Example from a course on pandas.
>
> - Tabular data: data with rows and columns
> - DataFrame: a pandas representation of tabular data
> - Merge: an opperation that combines two DataFrames based on common column values
> - Aggregate: a single value computed based on a set of values.  For example, an average applied to a column of numbers is an aggregate function

### E. What analogies or heuristics will you use?

- [ ] Write a list of analogies for concepts, heuristics for best practices, and any other non-technical explanations of things that may be helpful to students _(minimum of two)_.

>Example from a course on forecasting product demand. This analogy is likely intuitive to most people.
>
>Signal and noise - It's like trying to hear someone across a crowded room. Remove the noise, and you can easily understand what they are telling you.


### F. What mistakes or misconceptions do you expect? 

- [ ] Write a list of common mistakes _(minimum of two)_ that you think students will make. These can be programming mistakes, conceptual misunderstandings, or simply examples of things that are unintuitive. 

>Example from a course on generalized additive models:
>
>The difference between prediction intervals and confidence intervals.


### G. What datasets will you use? 

- [ ] Write a list of datasets that you will use in the course, a short description of each dataset (if it's not clear from the title), how you intend to use it and include a link to its source(s).

- [ ] Upload these datasets to your course on the Teach Editor. 

![teach_editor_datasets_upload](https://user-images.githubusercontent.com/20912644/44154482-d2e04b3a-a078-11e8-8ff9-2944fdcabeae.png)

[Sources of data](https://instructor-support.datacamp.com/frequently-asked-questions-faq/datasets/sources-of-data) to get you started. Please avoid these [overused datasets](https://instructor-support.datacamp.com/frequently-asked-questions-faq/datasets/datasets-to-avoid).

## Step 2: Who is this course for?

Terms like "beginner" and "expert" mean different things to different people, so we use personas to help instructors clarify a course's audience. When designing a specific course, instructors should explain how it will or won't help these people, and what extra skills or prerequisite knowledge they are assuming their students have above and beyond what's included in the persona.

- [x] Please select the roles and industries that align with your course. 
- [x] Include an explanation describing your reasoning and any other relevant information. 

### What roles would this course be suitable for?

*Check all that apply.*

- [ ] Data Consumer
- [ ] Leader 
- [ ] Data Analyst
- [ ] Citizen Data Scientist
- [x] Data Scientist
- [ ] Data Engineer
- [ ] Database Administrator
- [x] Statistician
- [ ] Machine Learning Scientist
- [x] Programmer
- [ ] Other (please describe)


- Data Scientist/Statistician: they may have many years of experience in working with R and/or Python packages, but haven't actually written their own
- Programmer: they might have written packages in other languages like Python, but haven't in R yet

### What industries would this apply to?

*List one or more industries that the content would be appropriate for.*

Finance, retail, medicine, transportation, communication, internet, entertainment, education, manufacturing, government, energy	

### What level of expertise should learners have before beginning the course?

*List three or more examples of skills that you expect learners to have before beginning the course*

- Experience navigating file and directory structures
- Experience writing and using R functions
- Experience working with package help documentation
- Experience reading through package vignettes

> Can draw common plot types (scatter, bar, histogram) using matplotlib and interpret them
> Can run a linear regression, use it to make predictions, and interpret the coefficients.
> Can calculate grouped summary statistics using SELECT queries with GROUP BY clauses.



## Step 3: Course outline

A typical course is structured as follows:

- Chapter 1 has three lessons. This chapter is shorter than the rest since it serves as an introduction to the topic.
- Chapter 2 has 3-4 lessons.
- Chapter 3 has 3-4 lessons.
- Chapter 4 has 3-4 lessons.

A typical lesson is comprised of:

- A video exercise with slides and script, e.g. [sample video exercise](https://campus.datacamp.com/courses/introduction-to-the-tidyverse/data-wrangling-1?ex=4).
- 2-4 exercises that review what is covered in the video exercise.

*Remind yourself about [course terminology](https://authoring.datacamp.com/courses/design#terminology-and-structure), then describe the flow of the course.*

- [ ] Does each lesson have a clear learning objective?
- [ ] Does each lesson include a brief list of functions or packages that the student will use?
- [ ] Does the outline have at least 12 lessons and no more than 15?

> CHAPTER 1: Baby's First Package
>   * Lesson 1.1: Simple package creation
>     - LEARNING OBJECTIVES: 
>         - Learn how to create the package structure for a package from R
>         - Learn how to make an R Markdown template file
>         - Learn how to include data in your package
>         - Learn how to check to see if your package template can be used
>     - FUNCTIONS introduced/used: `usethis::create_package()`, `usethis::use_rmarkdown_template()`, `usethis::use_data_raw()`, `usethis::use_data()`, `devtools::install()`, `rmarkdown::draft()`
>   * Lesson 1.2: Understanding the package structure basics
>     - LEARNING OBJECTIVES:
>          - Learn how to explain the components of a DESCRIPTION file
>          - Learn how to explain the components of a NAMESPACE file
>          - Learn how to describe the structure of the `R` directory
>          - Learn how to describe the structure of the `man` directory
>         - Learn what the optional R package directories are and how they are used
>     - Conceptual Exercises used here
>   * Lesson 1.3: Building package functions
>     - LEARNING OBJECTIVES: Refresh knowledge of writing R functions
>     - FUNCTIONS introduced/used: ``base::`function`()``
>
> CHAPTER 2: Package Workflows and Best Practices
>   * Lesson 2.1: More package structure
>     - LEARNING OBJECTIVES:
>          - Learn how to structure code and functions in the `R` directory
>          - Learn best practices on naming your package
>          - Learn how to see if your package name is available on CRAN
>          - Learn how to identify the different license options available for sharing your package
>     - FUNCTIONS introduced/used: Conceptual exercises and `available::available()`, `usethis::use_mit_license`, `usethis::use_cc0_license()`
>   * Lesson 2.2: Package build
>     - LEARNING OBJECTIVES:
>           - Learn how to load all files in your package to begin testing your package functionality
>           - Learn how to check that your package meets the requirements of R packages programmatically
>           - Learn how to install your package locally
>     - FUNCTIONS introduced/used: `usethis::use_r()`, `devtools::load_all()`, `devtools::check()`, `devtools::install()`
>   * Lesson 2.3: Please list all your dependencies
>     - LEARNING OBJECTIVES:
>           - Learn the different types of dependencies
>           - Learn how to add CRAN packages as dependencies to your package
>     - FUNCTIONS introduced/used: `usethis::use_package()`
>
> CHAPTER 3: The Power of Documentation
>   * Lesson 3.1: Help me help you
>     - LEARNING OBJECTIVES:
>         - Learn how to decipher the components of a help file with roxygen header documentation for a particular function
>         - Learn the differences between exported and non-exported functions
>         - Learn how to set a function as exported
>     - FUNCTIONS introduced/used: Conceptual exercises
>   * Lesson 3.2: Roxygen-ing into the night
>     - LEARNING OBJECTIVES:
>         - Learn how to create examples in roxygen
>         - Learn how to document a package and the smallest amount of detail needed to do so
>     - Functions introduced/used: Conceptual exercises and `roxygen2::roxygenize()`
>   * Lesson 3.3: Vignette, vidi, vici
>     - LEARNING OBJECTIVES:
>          - Learn how to create a vignette in the `vignettes` directory
>          - Learn how to add necessary vignette dependencies to `DESCRIPTION`
>          - Learn the best practices on building the vignette
>          - Learn how to read the vignette's metadata
>          - Learn the difference between a vignette and an article
>          - Learn how to build vignettes individually and all at once
>     - FUNCTIONS introduced/used: `usethis::use_vignette()`, `rmarkdown::render()`, `devtools::build_vignettes()`
> 
> CHAPTER 4: Ready for Testing
>   * Lesson 4.1: Unit tests
>     - LEARNING OBJECTIVES:
>          - Learn what a unit test is and why they are important to package development and maintenance
>          - Learn how to create a test file in the appropriate path
>     - FUNCTIONS introduced/used: `usethis::use_testthat()`, `usethis::use_test()`
>   * Lesson 4.2: Great expectations
>     - LEARNING OBJECTIVES:
>         - Learn what the most often used expect statements are in testing
>         - Learn how to check if two R objects are the same in value, attribute, and type 
>         - Learn how to test for R function calls producing specific or non-specific errors
>         - Learn how to test for R function calls producing specific or non-specific messages
>         - Learn how to test output of an R function matching the given output
>     - FUNCTIONS introduced/used: `testthat::expect_identical()`, `testthat::expect_equal()`, `testthat::expect_equivalent()`, `testthat::expect_error()`, `testthat::expect_warning()`, `testthat::expect_output()` 
>   * Lesson 4.3: More testing will be needed
>     - LEARNING OBJECTIVES:
>         - Learn how to organize tests into a single `testthat` function call
>         - ~Learn about what is meant by a testing context~ `context()` is no longer recommended
>         - Learn how to run unit tests and how to decipher the output
>     - FUNCTIONS introduced/used: `testthat::test_that()`,  `testthat::test_example()`, `testthat::test_file()`, `testthat::test_package()`

> Wrap-up video: Learn how to share your package on GitHub, how to use continuous integration with your package, and how to use `pkgdown` to build a website for your package.

## Step 4: Capstone exercises

Create a capstone exercise for **each chapter**  of your course in the Teach Editor. **(Note: This is different from what you did when you submitted your course outline application, which was just one exercise for the entire course)** Let your Curriculum Manager know when you have completed this step so that they can review your exercises and provide you with feedback.

A capstone exercise should showcase how far learners are likely to get at the end of each chapter. Please check out more details on this and further tips [here](http://instructor-support.datacamp.com/courses/course-design/building-capstone-exercises).

In addition to using the `Coding` exercise type, we expect you to use the `Iterative` and `Sequential` exercise types. Be sure to check out our documentation on [Iterative](https://instructor-support.datacamp.com/articles/2375528-course-iterative-exercises) and [Sequential](https://instructor-support.datacamp.com/articles/2375525-course-sequential-exercises) exercises before you start.

Please ensure that your capstone exercises meet our [content guidelines](https://instructor-support.datacamp.com/en/articles/2360978-content-guidelines).

## Step 5: Build ONE complete lesson on the Teach Editor

This should include:
1. A video exercise with slides (this can be the same as or similar to slides/video that you already created for your audition).
2. Between 2 and 4 exercises that allow students to practice what you taught in the video exercise.  This **does not** include Solution Correctness Tests (SCTs), but **does include** the success message for each exercise.


Why create a lesson as part of your course spec?

It will:

- Allow you to become familiar with the Teach Editor along with our different exercise and slide types earlier.
- Give you a better understanding of course scope (e.g., what can be covered in a reasonable amount of time, and what must be saved for a future course - compared to creating just a course outline.)

In combination, this will result in faster course development time, a more frictionless course development experience, and prevent roadblocks that arise out of miscalibrated course scope.

Our experience working with over a hundred expert instructors over the past 4 years has taught us that the most challenging part of creating a DataCamp course is in understanding the scope of what can be covered in a lesson (and, by extension, a course). 

We believe that students learn best when their hands are on the keyboard, writing code, working with data, and solving problems. Consequently, our courses consist of short  3 to 4-minute videos separated by interactive coding exercises, with occasional multiple choice exercises interspersed. The videos are intended to teach students the concepts necessary to solve the exercises that follow. 

You can read through all of our content guidelines [here](https://authoring.datacamp.com/courses/guidelines/content.html).

Four-minute videos correspond to between **400 and 600 words total** in the script. 

**Teaching data science concepts in this amount of time is not easy:** 
- It forces you to drill down to the essence of the concept and eliminate everything extraneous. 
- It requires a different approach compared to giving 50-minute college lectures.
- Writing such a script as part of your sample lesson will help you in creating a course outline that covers the right amount of content.

All lessons MUST follow our [content guidelines](https://authoring.datacamp.com/courses/guidelines/content.html).

## Lesson Rubric and Process

### Process

**Timeline**

Please work with your Curriculum Manager to ensure that all of the following boxes are checked. Once that happens, the Content Development team will review the lesson within **3 working days**, and you must incorporate the feedback (if any!) within the next **3 working days.** 

**Feedback Delivery**

There will be no more than **2 rounds of feedback** by a Content Developer, and in each round of feedback, the Content Developer will be specific and unambiguous in explaining exactly what revisions are necessary before the course can be considered ready for handoff. If, after two rounds of feedback, the lesson is still not deemed acceptable by the Content Development team, DataCamp will not move forward with course development.

### Lesson Rubric

#### General

- [ ] Does the lesson consist of 1 video followed by 2-4 exercises?
- [ ] Are there at least 2 coding exercises?
- [ ] Is there no more than 1 multiple choice exercise?
- [ ] Is the script for the video between 400 and 600 words?
- [ ] Are the titles of the exercises and slides written in sentence case?
- [ ] Do all of the exercises run on DataCamp in less than 3 seconds?
- [ ] Does the build pass?

#### Video

- [ ] Are the slides dynamic? That is, is there movement on the slides, such as in the form of transitions between bullets and lines of code, or progression through a visual/schema?
- [ ] Are full sentences in slides avoided?
- [ ] Is there a clear learning objective and/or narrative that motivates why the concept is important?
- [ ] Is there code in the slides? Learning by Doing requires Teaching by Doing!
- [ ] Does the code incorporate a relevant dataset that is [not overused](https://authoring.datacamp.com/courses/design/brainstorming-datasets.html)?
- [ ] Is the code properly formatted and placed inside backticks? It is your responsibility to ensure that your slides are properly formatted.
- [ ] Is the (trans)script written in complete sentences, without any bullet points or markdown? The script should correspond to exactly what you will say in the final recording and will be used to generate the subtitles for your course.

#### Exercises

- [ ] Are the [Content Guidelines](https://authoring.datacamp.com/courses/guidelines/content.html) met?
- [ ] Context: 180-780 characters
- [ ] Instructions: 1-4 bulleted instructions
- [ ] Hints: 1-4 bulleted hints
- [ ] Sample/Solution code: Less than or equal to 15 lines of code
- [ ] Success Message: Is there an informative success message?
- [ ] Do the comments in the sample and solution code match?
- [ ] Are the comments abbreviated instructions?
- [ ] Are the comments free of backticks?
- [ ] Is each comment less than 60 characters of length?
- [ ] Does each comment start with a space?
- [ ] Are different sections of code properly spaced?
- [ ] Are the instructions bulleted?
- [ ] Are the hints bulleted?
- [ ] Is the sample code appropriately scaffolded? R courses use 3 underscores.

#### FAQs

##### Which lesson should I create for my sample lesson?

This is your choice. We recommend the final lesson of your course, as it has the following advantages:

- The concepts will likely be more advanced, and confirming that you can adequately teach the material in less than 600 words will verify that the course scope is appropriate.
- Similarly, the code will tend to be more advanced and computationally intensive. Confirming that the code runs on DataCamp and that the exercises meet our content guidelines will provide another check to verify that the course scope is indeed appropriate. 
- It provides clarity on where students will be at the end of the course and a clear stopping point that you can then work towards during the rest of course development.

**Whichever lesson you create, it is important to keep in mind the spirit of the sample lesson:** 
- it is an important check on course scope, 
- an opportunity to acquaint yourself with the tools you will be using to build your course, 
- and a chance to receive early feedback on teaching style to ensure you and DataCamp are aligned on course vision.

## Step 6: Revisit course outline

Having created your sample lesson, you should now have a much better understanding of course scope. This is an ideal time to revisit your outline and update it if necessary.

## Step 7: Write course description and list course prerequisites

**Course Description**

Add a one-paragraph description of the course in the Settings tab of the Course Editor. How to get to editing this is shown below as an image.

![](https://user-images.githubusercontent.com/9215614/52296764-04f95c80-2934-11e9-9ff7-4ae5155ef189.png)

Please review these guidelines when creating it: <https://instructor-support.datacamp.com/courses/course-design/datacamp-course-descriptions>. 

The tidyverse includes a tremendous set of packages that make working with
  data simple and fast. But have you ever tried to put dplyr functions inside
  functions and been stuck with strange errors or unexpected results? Those
  errors were likely due to tidy evaluation, which requires a little extra work
  to handle. In programming with dplyr in R, you’ll be equipped with strategies
  for solving these errors via the rlang package. You’ll also learn other
  techniques for programming with dplyr using data from the World Bank and
  International Monetary Fund to analyze worldwide trends throughout. You’ll be
  a tidyverse function writing ninja by the end of the course!

The R package ecosystem is immense and can be intimidating to hop into.   Fear not! Developing your own R package is a valuable skill, even if you never work with another collaborator on your code. It's also nowhere near as challenging as it used to be with a suite of modern tools at your disposal. Creating a package allows you to document your functions to improve their quality, to have a formal structure for your code and analyses, and to allow you to improve your functions while letting tests ensure that you haven't broken previous functionality. You'll learn all about these ideas and create your own package along the way in this course!
  

> An example from a course analyzing survey data

> You've taken a survey (or 1000) before, right? Have you ever wondered
  what goes into designing a survey and how survey responses are turned into actionable
  insights? Of course you have! In Analyzing Survey Data in R, you will work with
  surveys from A to Z, starting with common survey design structures, such as clustering
  and stratification, and will continue through to visualizing and analyzing survey
  results. You will model survey data from the National Health and Nutrition Examination
  Survey using R's survey and tidyverse packages. Following the course, you will be
  able to successfully interpret survey results and finally find the answers to life's
  burning questions!

**Prerequisites**

*Which DataCamp courses cover topics that a student should be familiar with before attempting this course? Here are some examples:*

- [Introduction to Writing Functions in R](https://learn.datacamp.com/courses/introduction-to-writing-functions-in-r)

# Capstone exercise

Congrats on making it to the last exercise in the course! You'll now use your knowledge of `testthat` to run all tests in your package.  Try it out!

The `testthat` package has also been loaded for you in this exercise.

<!-- Note this might be a multi-part exercise with this being the last step in the exercise. A previous step in the exercise could be to check all tests in a file and then this would test across multiple files. -->

`@instructions`
- Run all tests for the package via a single function call.

`@solution`
```{r}
# Define custom function
test_package("dcPackage")
```

<!-- Depending on the output of the run here, there will likely be a multiple choice exercise to compare this output to a previous exercise that had some errors that we needed to fix. -->
