# bivariate_bubbles

*Currently just an idea*

`bivariate_bubbles` seeks to visualize a continuous quantity (cost, revenue,
profit, event rate, residual with respect to an existing prediction, ...)
aggregated with respect a pair of explanatory variables in a two-dimensional
bubble plot.  The size of a bubble will denote the number of observations for
the corresponding subset of the bivariate sample space; the color of a bubble
will denote the mean value of the continuous quantity. This can be a core
monitoring tool (as well as a tool in the development of predictive models),
going a step deeper than univariate charting (which on its own is crucial) to
display bivariate associations. For monitoring, this kind of visualization
could quickly isolate the portions of a system or book of business where there
are surprises or problems.

Some early architecture notes:

- Working in the Python data science ecosystem may be good for adoption.
- But something else under the hood (e.g., Rust with parallelism) may be good
  for processing larger data.
- The definition of buckets is expected to be deferred to `mixed_summary`,
  allowing for robust extraction of exceptional values for the explanatory
  variables.
- It would be nice to have an open format for the results of aggregation and to
  deliver the viz through that format. Say one has a 1 GB data set with ten
  columns, each of which corresponds to ten buckets. There are 10-choose-2 = 45
  bubble plots; each has 200 pieces of data (100 means and 100 counts).
  Ignoring bucket and plot metadata, that's 45 * 200 * 8 bytes per piece of
  data =  72 KB of bubbles. This huge compression of data (not to mention
  memoization of moderately expensive processing) would allow a browser to
  quickly render plots on demand and to have a convenient archival version of
  the results of analysis.
- Color scale should be configurable. I'd like to be able to plot various data
  with `bivariate_bubbles`. Diverging color, thresholded with white at zero,
  with a requested (corporate) visual identity would be a really common one.
  But sequential color (e.g., for always-positive quantities) or
  colorblind-friendly color schemes should be easily patched in.
- I don't want to build this over `matplotlib`. It's horrible pain to format
  `matplotlib` plots with features like titles and axes for an arbitrarily
  dimensioned plot. I want something, instead, from the modern JS-based viz
  ecosystem that can deliver viz either interactively to the browser or to
  static files. I recall [`vega-lite`](https://vega.github.io/vega-lite/)
  having shockingly concise specifications for moderately complex plots. One
  example showed a GitHub-style punchcard, which gets close to the desired
  bivariate bubble plot. I wonder if this stack is expressive enough to include
  the plot elements I'd need. We'll see.
- Math to sort plots by their "expressiveness" may belong here but may belong
  elsewhere. An "expressive" plot explains a lot of the variation in the
  continuous quantity via the colors of bubbles but without having too many
  bubbles. Math like the Bayes Information Criterion can provide a disciplined
  measure of expressiveness. I think I'll need to build some of this tool to
  have a good feeling for whether such analytics belong here.
