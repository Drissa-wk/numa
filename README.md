
# numa

numa is an R package for numerical analysis. It provides a collection of essential functions for numerical computations, including calculating derivatives, evaluating mathematical functions, manipulating polynomials, approximating functions using Taylor series expansion, numerical intégrations.

## Installation

You can install the development version from GitHub using the `devtools` package:

```R
devtools::install_github("Drissa-wk/numa")
```
## Functions
- 'combinaison'
- 'f'
- 'f_n_deriv'
- 'polyadd'
- 'polymul'
- 'polypow'
- 'polyprint'
- 'taylor'
- 'newton_raphson'
- 'trapezoidal_rule'
- 'rectangle_rule'



## Examples

Here are some examples demonstrating the usage of the Numa package functions:

```R
library(Numa)

# Calculate the binomial coefficient
combinaison(2, 5) # Returns 10

# Evaluate a mathematical function
f("exp(x)", 5) # Returns the value of exp(5)

# Calculate the nth derivative of a function
f_n_deriv('exp(x)', 2, 1) # Returns the second derivative of exp(x) at x = 1

#approximate a function using Taylor series expansion
taylor('log(sqrt(x))', n=5, a=1, pol=TRUE)
```

For more information on each function and its usage, refer to the   [numa website ](https://numa-nu.vercel.app).

## Contributing

Contributions to Numa are welcome! If you find any issues or have suggestions for improvements, please open an issue or submit a pull request on the [GitHub repository](https://github.com/Drissa-wk/numa).



