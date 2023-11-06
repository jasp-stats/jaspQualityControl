Response surface methodology
==========================
A response surface experiment extends the screening design to allow estimation of quadratic effects. After resolving confounding, the screening experiment allows coefficient estimates for main effects and interactions. The response surface experiment expands this equation by adding quadratic or even cubic terms. The form of the empirical model with quadratic terms is:

*y = c<sub>0</sub> + c<sub>1</sub>X<sub>1</sub> + c<sub>2</sub>X<sub>2</sub> + ... + c<sub>n</sub>X<sub>n</sub> + c<sub>12</sub>X<sub>1</sub>X<sub>2</sub> + c<sub>13</sub>X<sub>1</sub>X<sub>3</sub> + ... + c<sub>(n-1)(n)</sub>X<sub>n-1</sub>X<sub>n</sub> + c<sub>11</sub>X<sub>1</sub><sup>2</sup> + c<sub>22</sub>X<sub>2</sub><sup>2</sup> + ... + c<sub>nn</sub>X<sub>n</sub><sup>2</sup>*

Estimating the coefficients of the model shown in the above equation requires an experiment with at least three factor levels. A full factorial experiment with three levels includes enough trials to estimate all interactions, but quickly becomes large as the number of factors increases.

Full factorial experiments include all interactions. The hierarchical ordering principle and the effect heredity principle imply that higher order interactions may be ignored, and that any interaction will not be statistically significant at a level of 5% unless the corresponding main effects are also statistically significant at a level of 5%. This allows accurate predictions with 2nd or 3rd order polynomials which can be created with significantly less trials than a full factorial experiment.

## Central composite design
-------
*Central composite designs* (CCD) were first proposed by Box and Wilson for building a second order (quadratic) model for the response variable without needing to use a complete three-level factorial experiment. A central composite design contains an imbedded factorial (or fractional factorial of resolution V) design with nF factorial points and with nC centre points augmented with a group of star (axial) points that allows estimation of curvature. A central composite design always contains twice as many star (or axial) points (2k) as there are factors (k) in the design. These designs are constructed from three main parts, each of which comprises a set of experimental points. The three parts can be characterised as:

a)	The 2k vertices (±1, ±1, ..., ±1) of a *k*-dimensional "cube";

b)	The 2k vertices (±α, 0, 0, ..., 0), (0, ±α, 0, 0, ..., 0), ..., (0, 0, ..., 0, ±α) of a *k*-dimensional "star";

c)	A number of centre points (0, 0, ..., 0).

Part a) is simply a 2<sup>k</sup> factorial design or a fractional factorial design. The variable coding is chosen so that the two levels (low and high) are -1 or +1. This part is often referred to as "cube" whatever the number of variables may be.

Part b) consists of pairs of experimental points on the coordinate axes all at a distance α from the origin. The quantity α is often a value between 1.4 and 2 but is dependent on the number of variables and the properties of the design under study. This part is often referred to as "star" or "axial".

![An image of the rsm design shapes: cube, star, and cube + star](%HELP_FOLDER%/images/doeResponseSurfaceMethodology/rsmDesignShapes.JPG)
 
There are three general types of central composite designs:

### Central composite circumscribed (CCC)
CCC designs are the original form of the central composite design. The star (or axial) points are located on a circle encompassing the screening design. This is shown in using -1 and +1 as the low and high levels of the original screening design with two factors. These designs have circular, spherical, or hyper-spherical symmetry and require five levels for each factor. It may not be possible to augment a screening design, represented by the corners of the square, if the levels of the screening design are absolute minimum and maximum as the star (or axial) points exceed these values.

### Face centred central composite (CCF)
In the CCF design the star points are located at the centre of each face, i.e. at the middle lines of the square (for two factor designs) or at the centre of the square of the cube (for three factors) formed by the screening design. This variety requires three levels of each factor. This design is not rotatable, and has slightly less precision for coefficient estimation. Its advantage is that star (or axial) points can be added to prior screening designs.

### Central composite inscribed (CCI)
For those situations in which the limits specified for factor settings are truly limits, the CCI design uses star (or axial) points which are set at the minimum and maximum levels. If augmenting a screening design, the levels of the screening design will be within the absolute limits to allow the expanded settings for the star (or axial) points. This design also requires five levels of each factor.

![An image of the CCD types: circumscribed, face-centered, and inscribed.](%HELP_FOLDER%/images/doeResponseSurfaceMethodology/rsmCCDTypes.JPG)

## Box-Behnken design
-------

A *Box-Behnken design* (BBD) is an independent quadratic design in that it does not contain an embedded factorial (or fractional factorial) design i.e., all factors are never set at their high levels simultaneously. In this design the treatment combinations are at the midpoints of edges of the process space and at the centre. These designs are rotatable (or near rotatable) and require three levels of each factor. The designs have limited capability for orthogonal blocking compared to the central composite designs.

![An image of the BBD.](%HELP_FOLDER%/images/doeResponseSurfaceMethodology/rsmBBD.JPG)

The Box-Behnken set-up consists of twelve "edge" points (shown as red dots) all lying on a single sphere about the centre of the experimental region (shown as a blue dot), plus a few replicates of the centre point, e.g., three (blue) centre points.

## References 
-------
- Dodson, L., Dodson, M., and Klerx, R. (2010), *A study of the hierarchical ordering principle and the effect heredity principle in factorial experimental designs*, 10th Annual Transaction of ENBIS (12 16 September 2010, Antwerp).
- Box, G.E.P., and Wilson, K.B. (1951), On the experimental attainment of optimum conditions. *Journal of the Royal Statistical Society 13*(1), 1-45.
- Dodson, B., Weidenbacher, M., Lynch D., and Klerx, R., QT 9 - Design and analysis of experiments. *SKF Quality Techniques*. (PUB GQS/P9 16083 EN - August 2015).
