---
layout: page
title: Slides and Class Content
permalink: /teaching/soc301-f2016/slides/week01/
---

## Week 1

### Wednesday
- <a href = "{{ site.baseurl }}/teaching/soc301-f2016/slides/week-01/01b.html">2016-08-31</a>

#### Covered content
- Go over PS1
    - FAQ [here]({{ site.baseurl }}/teaching/soc301-f2016/ps-faq/PS1/)
- Log-in to RStudio Server
    - **Make sure to switch from R 3.1.0 to R 3.2.0 in the top right hand part of the screen**
    - Install the needed packages
    
    ```r
    needed_pkgs <- c("nycflights13", "dplyr", "ggplot2", "knitr",  "ggplot2movies", "dygraphs", 
                     "rmarkdown", "mosaic", "tibble")

    new.pkgs <- needed_pkgs[!(needed_pkgs %in% installed.packages())]

    if(length(new.pkgs)) {
      install.packages(new.pkgs, repos = "http://cran.rstudio.com")
    }
    ```

- If everything installed correctly, you'll see this when you run the code again.  If you don't get this, [send me an email](mailto:chester@pacificu.edu)   

![Install Went Well]({{ site.baseurl }}/teaching/soc301-f2016/slides/install_pkg.gif)

- Read over Chapters 3, 4, and 5 in [Getting Used to R](http://ismayc.github.io/rbasics-book)
    
***

### Monday
- <a href = "{{ site.baseurl }}/teaching/soc301-f2016/slides/week-01/01a.html">2016-08-29</a>

#### Covered Content
- Introduction to the course and to each other
- [Pre-test](https://www.surveymonkey.com/r/XSYDHJB) Due by 3:00 PM Tuesday
- [PS1](https://goo.gl/forms/kOCJIEMpS1i8lqgn1) Due by 10:00 AM on Wednesday
- Read the first two chapters of [Getting used to R, RStudio, and RMarkdown](http://ismayc.github.io/rbasics-book) for Wednesday
- Read the first two chapters of [A MODERN DIVE into Data with R](https://ismayc.github.io/moderndiver-book/) for Wednesday