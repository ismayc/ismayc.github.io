# MATH 141
Chester Ismay  






## Example from last time {.build}

How many quizzes should we have printed in total for MATH 141 today?

- The Registrar at Reed has told us that students tend to come to class about 93% of the time
- There are 38 students registered in total
- What specific assumptions are required for us to assume a Binomial/Independent Trials model?
    - Students come to class _independently_ of one another
    - Each student has a 93% chance of attending class today
    - Students either come to class or they don't
- What does this distribution look like?
    - http://ismay.shinyapps.io/ProbApp

# Poisson Distribution | How _weird_ are Obama's three Supreme Court Justice nominations?

## {.flexbox .vcenter}

$X$ - The number of Justice nominations for US two (full) term Presidents can be modeled using a Poisson Distribution ($X \sim Pois(\lambda = 3.54)$)

<img src="../figs/justices.png" alt="Justices" width="700">

## So what's a Poisson distribution?

- Originated in an attempt to find a simple approximation for binomial probabilities in situations where
    - there are a large number of independent trials
    - each with a small probability of success
- It is a probability model for __rare events__
- Other examples:
    - $C$ = The count of salamanders found in a 10 meter square region of the Reed canyon during a 1 hour period
    - $P$ = The number of incoming phone calls at the Reed switchboard in a 10 minute period
