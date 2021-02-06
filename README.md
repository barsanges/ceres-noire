# Cérès noire

`Cérès noire` is a tool to help you choose the least costly combination of
stamps for a letter.

**The postage cost problem** Given a set of ![n](https://render.githubusercontent.com/render/math?math=%5Cdisplaystyle+n)
stamps, each with a cost ![c_i](https://render.githubusercontent.com/render/math?math=%5Cdisplaystyle+c_i)
and available in a limited quantity ![{\bar x}_i](https://render.githubusercontent.com/render/math?math=%5Cdisplaystyle+%7B%5Cbar+x%7D_i),
an *admissible solution* to the problem is a vector ![x](https://render.githubusercontent.com/render/math?math=%5Cdisplaystyle+x) such that:

![\sum_i c_i x_i \geq C \: \text{and} \: \forall i, x_i \leq {\bar x}_i](https://render.githubusercontent.com/render/math?math=%5Cdisplaystyle+%5Csum_i+c_i+x_i+%5Cgeq+C+%5C%3A+%5Ctext%7Band%7D+%5C%3A+%5Cforall+i%2C+x_i+%5Cleq+%7B%5Cbar+x%7D_i)

The *boundary of the set of admissible solutions* is defined as:

![\mathcal{B} = \{ x / \sum_i c_i x_i \geq C \: \text{and} \: \forall i, x_i \leq {\bar x}_i \: \text{and} \:  \forall i, c_i (x_i - 1) + \sum_{j \neq i} c_j x_j < C \}](https://render.githubusercontent.com/render/math?math=%5Cdisplaystyle+%5Cmathcal%7BB%7D+%3D+%5C%7B+x+%2F+%5Csum_i+c_i+x_i+%5Cgeq+C+%5C%3A+%5Ctext%7Band%7D+%5C%3A+%5Cforall+i%2C+x_i+%5Cleq+%7B%5Cbar+x%7D_i+%5C%3A+%5Ctext%7Band%7D+%5C%3A++%5Cforall+i%2C+c_i+%28x_i+-+1%29+%2B+%5Csum_%7Bj+%5Cneq+i%7D+c_j+x_j+%3C+C+%5C%7D)

An *optimal solution* (if any) is:

![\min ( \mathcal{B} )](https://render.githubusercontent.com/render/math?math=%5Cdisplaystyle+%5Cmin+%28+%5Cmathcal%7BB%7D+%29)