# Cérès noire

`Cérès noire` is a tool to help you choose the least costly
combinations of stamps for a letter.

**The postage cost problem** Given a set of $n$ stamps, each with a
cost $c_i$ and available in a limited quantity ${\bar x}_i$, an
*admissible solution* to the problem is a vector $x$ of positive
integers such that:

$$C_1 \leq \sum_i c_i x_i \leq C_2 \: \text{and} \: \forall i, x_i \leq {\bar x}_i$$

where $C_1$ and $C_2$ are the lower and upper bounds of the problem.

A solution $x$ is a *superset* of another solution $y$ if and only if:

$$\forall i, x_i \geq y_i.$$

$x$ is a *strict superset* of $y$ if and only if $x$ is a superset of
$y$ and:

$$\sum_i x_i > \sum_i y_i.$$
