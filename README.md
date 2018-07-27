# impedanceR

## Description

This package contains a suite of functions for calculating and simulating electrical or electrochemical impedance spectra. The package contains functions for simulating the most common equivalent circuit elements, some convenient functions for constructing the circuits themselves, and other useful utility functions.

## Functions included so far

See the R documentation (i.e., `?functionname`) for more information on usage

### Equivalent circuit elements

`Z_R()` - resistor

`Z_C()` - capacitor

`Z_L()` - inductor

`Z_Q()` - constant phase element

`Z_W()` - Warburg element

`Z_FLW()` - Finite Length ("short") Warburg element (FLW)

`Z_FSW()` - Finite Space ("open") Warburg element (FSW)

`Z_G()` - Infinite length Gerischer element

`Z_FLG()` - Finite length Gerischer element

`trans_line()` - transmission line (for more complicated transmission line circuits)

### Other functions

`para()` - for adding any number of circuit elements in parallel

`omega_range()` - for calculating a range of angular frequency values (necessary for all circuit elements, except resistors)

`Z_to_df()` - convert a complex vector to a `data.frame` of real and imaginary values

## Example usage

Construction of equivalent circuits is aimed to be as simple as possible. For example; a simple series RC circuit can be simulated as follows:

```{r}
Z_R(R = 10) + Z_C(C = 1E-5, omega = omega_range(low.logf = -1, high.logf = 5, p.per.dec = 10))
```

This can of course be simplified to:

```{r}
Z_R(10) + Z_C(1E-5, omega_range(low.logf = -1, high.logf = 5, p.per.dec = 10))
```

Or the elements can be defined separately and combined later:

```{r}
R1 = Z_R(10)
C1 = Z_C(1E-5, omega_range(low.logf = -1, high.logf = 5, p.per.dec = 10))

R1 + C1
```

A parallel RC circuit can be easily calculated using the `para()` function:

```{r}
para(R1, C1)
```

And more complex circuits are easily created by combining these functions:

```{r}
R2 = Z_R(25)

R1 + para(C1, R2)
```

For circuits with multiple frequency-dependent elements, the `omega` argument in each element must be identical - currently the functions do not check for this. This is best handled by creating a separate variable with the desired range of values:

```{r}
omega = omega_range(low.logf = -1, high.logf = 5, p.per.dec = 10)
C1 = Z_C(1E-5, omega)
C2 = Z_C(1E-6, omega)

para(R1, C1) + para(R2, C2)
```

Results can easily be converted into data frames and plotted:

```{r}
result <- R1 + para(C1, R2)
df <- Z_to_df(result)

library(ggplot2)

ggplot(df, aes(x = Re, y = -Im)) +
  geom_point() +
  coord_fixed()
```
