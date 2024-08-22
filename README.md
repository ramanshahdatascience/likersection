# likersection

*Under Construction*

`likersection` is an R package that mines bivariate insights from survey data.
It does this by aggregating a Likert-valued quantity (such as a satisfaction
measure) with respect to pairs of explanatory variables (such as demographic
measures) in a two-dimensional bubble plot. Likert-valued data are tricky to
analyze because they are partly ordinal ("Strongly Agree" to "Strongly
Disagree" in a finite number of notches) and partly not (answering "Don't Know"
or leaving the question blank). By defining a likelihood function aware of this
complexity, `likersection` is able to sort the N-choose-2 possible aggregations
by an Akaike Information Critierion, answering the question "Which intersection
of two explanatory variables best expresses the contrast in this quantity we're
interested in?"

`likersection` then visualizes such insights as a two-way bubble plot. The size
of a bubble denotes the number of observations for the corresponding subset of
the bivariate sample space; each bubble is made of a colored wedge (denoting
the mean value of the continuous quantity) and a gray wedge (denoting the
proportion of Don't Know/null/blank responses).
