# Cérès noire

`Cérès noire` is a tool to help you choose the least costly combination of
stamps for a letter.

**The postage cost problem** Given a set of ![n](https://render.githubusercontent.com/render/math?math=%5Ctextstyle+n)
stamps, each with a cost ![c_i](https://render.githubusercontent.com/render/math?math=%5Ctextstyle+c_i)
and available in a limited quantity ![{\bar x}_i](https://render.githubusercontent.com/render/math?math=%5Ctextstyle+%7B%5Cbar+x%7D_i),
an *admissible solution* to the problem is a vector ![x](https://render.githubusercontent.com/render/math?math=%5Ctextstyle+x) of positive integers such that:

![\sum_i c_i x_i \geq C \: \text{and} \: \forall i, x_i \leq {\bar x}_i](https://render.githubusercontent.com/render/math?math=%5Cdisplaystyle+%5Csum_i+c_i+x_i+%5Cgeq+C+%5C%3A+%5Ctext%7Band%7D+%5C%3A+%5Cforall+i%2C+x_i+%5Cleq+%7B%5Cbar+x%7D_i)

where ![C](https://render.githubusercontent.com/render/math?math=%5Ctextstyle+C)
is the cost of a letter.

An *optimal solution* (if any) is ![x](https://render.githubusercontent.com/render/math?math=%5Ctextstyle+x) such that, for all other admissible solution ![y](https://render.githubusercontent.com/render/math?math=%5Ctextstyle+y):

![\sum_i c_i x_i \leq \sum_i c_i y_i](https://render.githubusercontent.com/render/math?math=%5Cdisplaystyle+%5Csum_i+c_i+x_i+%5Cleq+%5Csum_i+c_i+y_i)