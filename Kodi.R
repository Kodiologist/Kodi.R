library(R.cache)
library(ggplot2)
library(grid) # For "unit"

# --------------------------------------------------
# Miscellany
# --------------------------------------------------

gimme = function(x)
# Like dput, but without the extra newlines.
  {cat(deparse(x))
   cat("\n")}

vassign = function(vars, values, envir = parent.frame())
# vassign(.(a, b), c(1, 2)) is equivalent to {a = 1; b = 2;}.
   {m = match.call()
    vassign.char(char(m$vars)[-1], values, envir)}

vassign.char = function(vars, values, envir = parent.frame())
    for (i in seq_along(vars))
        assign(vars[[i]], values[[i]], envir)

newseed = function()
# Sets the RNG seed with the current time including some fractional
# seconds.
    set.seed((as.numeric(Sys.time()) * 1e5) %% .Machine$integer.max)

showexprs = function(...)
# A debugging aid.
# $ x = 5
# $ showexprs(x, 2*x)
# x = 5
# 2 * x = 10
   {for (expr in match.call(expand.dots = F)$"...")
        {val = eval(expr, parent.frame())
         message(deparse(expr), " = ", val)}}

show.caches = function(path = ".")
# Use interactively in an R.cache directory to peek at its
# contents.
   {do.call(rbind, lapply(list.files(path, full.names = T), function(name)
        data.frame(name, readCacheHeader(name))))}

setup.cluster = function()
# Make a default parallel cluster if there isn't one already.
   {library(parallel)
    tryCatch(parallel:::defaultCluster(), error = function(e)
       setDefaultCluster(makeForkCluster(getOption("mc.cores", 2))))
    T}

# --------------------------------------------------
# Metaprogramming
# --------------------------------------------------

splice.into.expr = function(expr, l)
# Similar to `substitute`, but substitutions of curly-delimited
# blocks are spliced into the outer construct. Also, the arguments
# to `splice.into.expr` are evaluated.
#   splice.into.expr(
#      expr = quote(
#         {a = 1
#          if (q == x)
#             {b = 1
#              FOO
#              y = 1}
#          z = 1}),
#      l = list(FOO = quote(
#         {g = 2
#          h = 2})))
#    =>
#      {a = 1
#       if (q == x)
#          {b = 1
#           g = 2
#           h = 2
#           y = 1}
#       z = 1}
   {if (typeof(expr) == "symbol")
       {name = as.character(expr)
        if (name %in% names(l))
           if (typeof(l[[name]]) == "language" &&
                   identical(l[[name]][[1]], quote(`{`)))
               as.list(l[[name]])[-1]
           else
               l[[name]]
        else
           expr}
    else if (typeof(expr) == "language")
      {head = splice.into.expr(expr[[1]], l)
       tail = Reduce(c, lapply(as.list(expr)[-1], function(x)
           if (identical(x, substitute()))
             # We have to take a bit of extra care with the
             # missing symbol.
             # http://stackoverflow.com/questions/3892580
               list(substitute())
           else
              {v = splice.into.expr(x, l)
               if (typeof(v) == "list") v else list(v)}))
       as.call(c(list(head), tail))}
    else
       expr}

# --------------------------------------------------
# Functions
# --------------------------------------------------

comp = function(...)
# Function composition. Like functional::Compose, but applies its
# arguments in the reverse order.
   {fs = rev(list(...))
    function(...) Reduce(function(x, f) f(x), fs, ...)}

# --------------------------------------------------
# Environments
# --------------------------------------------------

env.chain = function(e)
   {if (identical(e, emptyenv()))
       environmentName(e)
    else
       c(environmentName(e), env.chain(parent.env(e)))}

copy.env = function(e, parent = parent.env(e))
   {new = list2env(as.list(e, all.names = T), parent = parent)
    attr(new, "name") = attr(e, "name")
    new}

# --------------------------------------------------
# Vectors generally
# --------------------------------------------------

ifna = function(a, b)
    ifelse(is.na(a), b, a)

maxby = function(v, f)
    v[[ which.max(sapply(v, f)) ]]

which.new = function(v)
# which.new(c(1, 1, 1, 2, 2, 2, 3, 3, 4, 4, 4, 4)) →
# c(1, 4, 7, 9)
   c(1, which(c(v, NA) != c(NA, v)))

pick = function(v, size = 1, ...)
# Like sample, but without surprising behavior when length(v) == 1
# and with size defaulting to 1.
    v[sample.int(length(v), size, ...)]

modnames = function(x, ...)
# modnames(mtcars, c = "carb", f = "disp") renames two columns
# without touching the others.
   `names<-`(x, ifna(
       `names<-`(names(c(...)), c(...))[names(x)],
       names(x)))

# --------------------------------------------------
# Lists
# --------------------------------------------------

punl = function(...)
# Like 'list', but uses punning to generate default names, so
#   punl(a, b, c = x, d)
# is equivalent to
#   list(a = a, b = b, c = x, d = d)
# As a bonus, 'punl()' is equivalent to
#   `names<-`(list(), character())
# That is, 'punl()' creates an empty named list.
   {m = match.call(expand.dots = F)
    inp = as.character(m$"...")
    l = list(...)
    names(l) =
        (if (is.null(names(l)))
            inp
         else
            ifelse(names(l) == "", inp, names(l)))
    l}

lapply.names = function(X, FUN, ...)
  {s = names(X)
   `names<-`(lapply(s, FUN, ...), s)}

# --------------------------------------------------
# Strings
# --------------------------------------------------

`%.%` = function(a, b)
    paste0(a, b, collapse = "")

qw = function(...)
# qw(a, b, c) → c("a", "b", "c")
   {m = match.call(expand.dots = FALSE)
    char(m[["..."]])}

char = as.character

ucfirst = function(s)
    ifelse(is.na(s), NA, paste(sep = "",
        toupper(substring(s, 1, 1)),
        substring(s, 2)))

# --------------------------------------------------
# Numbers
# --------------------------------------------------

num = as.numeric

div1 = function(v, n)
# div1(1:20, 4) =>
# 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5
    ((v - 1) %/% n) + 1
mod1 = function(v, n)
# mod1(1:20, 6) =>
# 1 2 3 4 5 6 1 2 3 4 5 6 1 2 3 4 5 6 1 2
    ((v - 1) %% n) + 1

logit = function(v)
    log(v / (1 - v))
ilogit = function(v)
    1 / (1 + exp(-v))

triangle = function(n)
# The nth triangle number.
   n * (n + 1) / 2

triangle.floor = function(n)
# Returns the index of the greatest triangle number
# less than or equal to n.
   {tni = floor(sqrt(2 * n)) + (-1:1)
    tni[which(triangle(tni) > n)[1]] - 1}

varn = function(v, na.rm = F)
# Variance with n instead of (n - 1) in the denominator.
    mean((v - mean(v, na.rm = na.rm))^2, na.rm = na.rm)
sdn = function(v, na.rm = F)
# Standard deviation with n instead of (n - 1) in the denominator.
    sqrt(varn(v, na.rm))

mean.dmin = function(drop, v)
# Mean of 'v' dropping the 'drop' least elements.
    mean( sort(v)[-(1 : drop)] )
mean.dmax = function(drop, v)
# Mean of 'v' dropping the 'drop' greatest elements.
    mean( sort(v, dec = T)[-(1 : drop)] )

qmean = function(v)
   {qs = as.vector(quantile(v, c(.025, .975)))
    c(lo = qs[1], mean = mean(v), hi = qs[2])}

mad1 = function(v, na.rm = FALSE)
    mad(v, na.rm, constant = 1)

sa0 = function(v)
    seq_along(v) - 1

recdf = function(v)
# Reverse empirical cumulative distribution function:
# recdf(c(2.999999, 3, 3.000001, 4))(3) == 0.75
   {f = ecdf(v)
    function(x) 1 - f(x - 2 * .Machine$double.eps)}

invscore = function(v)
    max(v) - v
center = function(v)
    v - mean(v)
zscore = function(v)
    (v - mean(v, na.rm = T))/sdn(v, na.rm = T)
rng = function(v)
   {r = range(v)
    r[2] - r[1]}
scale01 = function(v)
   {r = range(v)
    (v - r[1])/(r[2] - r[1])}

rnorm.f = function(n, mean = 0, sd = 1)
# Like rnorm, but the vector is scaled and shifted to have exactly
# the requested sample mean and unbiased estimator of the
# population SD.
    {v = rnorm(n)
     (v - mean(v)) / sd(v) * sd + mean}

modes = function(x, all = FALSE, ...)
# http://r.789695.n4.nabble.com/Statistical-mode-td3553534.html
{
  if(is.list(x))
  {
    output <- sapply(x, statmode, all=all, ...)
  }
  else
  {
    freq <- table(x, ...)
    if(all)
      output <- names(freq)[freq==max(freq)]
    else
      output <- names(freq)[which.max(freq)]
    ## Coerce to original data type, using any() to handle mts, xtabs, etc.
    if(any(class(x) %in% c("integer","numeric","ts","complex","matrix","table")))
      output <- as(output, storage.mode(x))
  }
  return(output)
}

mode.cont = function(v)
# Estimate the mode of a continuous variable. From
# http://r.789695.n4.nabble.com/how-to-calculate-the-mode-of-a-continuous-variable-td865503.html
   {dd = density(v)
    dd$x[which.max(dd$y)]}

kquant = function(v)
    quantile(v, c(.025, .1, .25, .4, .5, .6, .75, .9, .975))

abbcor = function(...)
# An abbreviated correlation matrix, which shows each
# correlation only once.
   {x = cor(...)
    x[!upper.tri(x)] = NA
    x[-nrow(x),-1]}

within1 = function(n)
# within1(5) → c(0.1, 0.3, 0.5, 0.7, 0.9)
   {x = 1/n
    v = seq(0, 1 - x, len = n)
    maprows(cbind(v, v + x), mean)}

normal.between = function(a, b, p)
# Computes the mean and SD of a normal distribution with
# its centered p-credible interval equal to [a, b].
    c(
        mean = mean(c(a, b)),
        sd = (b - a)/2/abs(qnorm((1 - p)/2)))

posix.ct = function(n)
# Converts a Unix time to a POSIXct.
    as.POSIXct(n, origin = "1970-1-1", tz = "UTC")

# --------------------------------------------------
# Factors
# --------------------------------------------------

logi2factor = function(v, labels)
   `levels<-`(factor(as.logical(v), levels = c(F, T)), labels)

int2factor = function(v, min.v, levels, ordered = F)
# int2factor(c(5, 0, 1, 3), 0, LETTERS[1:10]) =>
#   [1] F A B D
#   Levels: A B C D E F G H I J
   factor(levels[v - min.v + 1], levels = levels, ordered = ordered)

interact = function(...)
    interaction(lex.order = T, ...)

groups = function(v)
# Shows the values of a factor and how many cases have that value.
# (Deprecated in favor of base::table.)
   {g = c(by(v, v, length))
    ifelse(is.na(g), 0, g)}

modlevels = function(x, ...)
# modlevels(x, A = "B", Q = c("foo", "bar"), .NA = "baz")) renames
# levels "B", "foo", "bar", and "baz" without touching other levels.
   {old = levels(x)
    l = list(...)
    replaced = as.vector(c(l, recursive = T))
    old = old[!(old %in% replaced)]
    x[char(x) %in% l$.NA] = NA
    l$.NA = NULL
    `levels<-`(x, c(
        structure(as.list(old), .Names = old),
        l))}

fframe = function(...)
# fframe(c("a", "b", "c", "b"), c("q", "c", "p"), c("q", "r"))
# → data.frame(f = factor(c(1, 1, 1, 1, 2, 2, 2, 3, 3)),
#              v = c("a", "b", "c", "b", "q", "c", "p", "q", "r"))
# Useful for functions that want formulae, and for ggplot.
   {library(reshape2)
    a = melt(list(...))
    a[c(2, 1)] = a[c(1, 2)]
    a[[1]] = factor(a[[1]])
    names(a) = c("f", "v")
    a}

# --------------------------------------------------
# Data frames (and matrices)
# --------------------------------------------------

ss = subset

reprows = function(m, ...)
    m[rep(1 : nrow(m), ...),]

samprows = function(m, n, replace = F)
    m[sample.int(nrow(m), n, replace),]

dist.idx = function(d, i)
# Given a dist object and indices into its vector, returns
# the matrix coordinates. Adapted from: http://stackoverflow.com/a/12643509
   {n = attr(d, "Size")
    col = ceiling(n - (1 + sqrt(1 + 4 * (n^2 - n - 2*i)))/2)
    cbind(row = n - (2*n - col + 1) * col / 2 + i + col, col)}

which.nearest = function(m, ...)
# Returns the indices of the rows of m with the least distance.
#   m = matrix(rnorm(3000, sd = 5), ncol = 3, byrow = T)
#   m[which.nearest(m),]
   {d = dist(m, ...)
    dist.idx(d, which.min(d))}

which.farthest = function(m, ...)
# Returns the indices of the rows of m with the greatest distance.
   {d = dist(m, ...)
    dist.idx(d, which.max(d))}

rows.with.dist = function(m, a, ...)
# A generalization of which.nearest and which.farthest. With a =
# 1, finds the farthest two points; with a = 0, finds the nearest
# two points; and with a = .5, finds the two points with median
# distance.
   {d = dist(m, ...)
    targets = as.vector(quantile(d, a, type = 1))
    dist.idx(d, sapply(targets, function(t) which(d == t)[1]))}

expand.grid.idx = function(lengths, row, col)
# Given some coordinates in an expand.grid-generated matrix,
# returns the corresponding index of the original input vector.
# If    m = do.call(expand.grid, vs)
# then  m[r, c] =
#     vs[[c]][expand.grid.idx(c(length(v1), …, length(vN)), r, c)]
    mod1(div1(row, prod(head(lengths, col - 1))), lengths[col])

evq = function(quoted, x)
    plyr::eval.quoted(quoted, x, attr(quoted, "env"))[[1]]

ssq = function(x, quoted.subset)
# Lets you do stuff like:
#   carbs = function(p) ssq(mtcars, p)$carb
#   f = function(cyln) carbs(.(cyl == cyln))
#   f(6)
  {v = evq(quoted.subset, x)
   x[!is.na(v) & v,]}

sort.df = function(df)
    ordf(df, row.names(df))

ordf = function(`_data`, ...)
# Similar to plyr::arrange, but preserves row names.
# Data = ordf(Data, FactorB, -FactorA, …)
   {m = match.call()
    `_data`[eval(as.call(c(quote(order), as.list(m)[c(-1, -2)])),
        `_data`, parent.frame()),, drop = F]}

dfn = function(df)
    cbind(RNAME = row.names(df), df,
        stringsAsFactors = F)

ssnew = function(df, expr)
# ssnew(ordf(mtcars, carb), carb) yields the first car with each
# unique carb value.
   {m = match.call()
    df[which.new(do.call(with, list(df, m$expr))),]}

dby = function(.data, .by, ...)
# dby(mtcars, .(gear, vsam = vs & am), N, mpg = median(mpg), sum(cyl))
#      gear  vsam  N   mpg sum.cyl.
#    1    3 FALSE 15 15.50      112
#    2    4 FALSE  6 21.00       32
#    3    5 FALSE  4 17.75       26
#    5    4  TRUE  6 28.85       24
#    6    5  TRUE  1 30.40        4
# (The function name used for "by" is ignored.)
   {m = match.call(expand.dots = F)
    as = as.list(m$"...")
    pf = as.list(parent.frame())
    plyr::ddply(
        .data = .data,
        .variables = .by,
        function(slice)
            with(c(list(N = nrow(slice)), as.list(slice), pf),
                do.call(data.frame, as)))}

maprows = function(x, f) apply(x, 1, f)
mapcols = function(x, f) apply(x, 2, f)

widef = function(df)
# For printing wide data frames (or just wide columns).
    for (i in 1 : nrow(df))
       {cat(row.names(df)[i])
        cat(":: ")
        cat(sapply(df[i,], char), sep = ", ")
        cat("\n")}

df2nv = function(df)
# Converts a one- or two-column data frame to a named vector.
# df2nv(mtcars["cyl"]) → c("Mazda RX4" = 6, "Mazda RX4 Wag" = 6, …)
# df2nv(data.frame(x = letters, y = 1:26)) → c(a = 1, b = 2, …)
   {if (ncol(df) == 1)
        `names<-`(df[[1]], row.names(df))
    else
        `names<-`(df[[2]], df[[1]])}

# --------------------------------------------------
# Plotting
# --------------------------------------------------

kodi.theme = function(font.size = 24) theme_bw(base_size = font.size) + theme(
# A low-vision theme, with big text and black and white instead of
# grays.
    panel.border = element_rect(fill = NA, colour = "black"),
    plot.margin = unit(c(2, 2, 2, 2), "lines"),
    axis.title.y =
        element_text(vjust = 0, angle = 90),
    axis.title.x =
        element_text(vjust = -2),
    panel.grid.major =
        element_line(colour = "gray50", size = 0.15),
    panel.grid.minor =
        element_line(colour = "gray50", size = 0.15, linetype = 2))

no.gridlines = function() theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

ksyc = function(...) # Shorthand for scale_y_continuous(expand = c(0, 0), ...)
   {m = match.call(expand.dots = F)
    do.call(scale_y_continuous, args =
        c(as.list(m$"..."), list(expand = c(0, 0))))}

laes = function(...)
  # Use instead of aes(...) to evaluate the right-hand sides of
  # mappings when the outer expression is evaluated.
   `class<-`(list(...), "uneval")

meanboot = stat_summary(
    fun.data = "mean_cl_boot",
    geom = "crossbar",
    color = "blue", width = .1)

boxp = geom_boxplot(
    fill = NA, color = "blue", outlier.colour = NA)

plotfun = function(f, xrange, yrange, n = 101)
    qplot(xrange, yrange, geom = "blank") +
        stat_function(fun = f, n = n) +
        scale_x_continuous(name = "", expand = c(0, 0)) +
        scale_y_continuous(name = "", expand = c(0, 0)) +
        coord_cartesian(xlim = xrange, ylim = yrange)

plotfuns = function(fs, xrange, yrange, n = 101)
   {xx = seq(xrange[1], xrange[2], len = n)
    d = do.call(rbind, lapply(seq_along(fs), function(i)
        data.frame(f = i, x = xx, y = fs[[i]](xx))))
    d$f = factor(d$f)
    qplot(xrange, yrange, geom = "blank") +
        geom_line(aes(xx, y, color = f), data = d) +
        scale_color_hue(l = 30) +
        scale_x_continuous(name = "", expand = c(0, 0)) +
        scale_y_continuous(name = "", expand = c(0, 0)) +
        coord_cartesian(xlim = xrange, ylim = yrange)}

dodge = function(x, y, faceter = NULL, data = NULL, discrete = F)
   {m = match.call()
    x = as.factor(eval(m$x, data, parent.frame()))
    y.orig = eval(m$y, data, parent.frame())
    y = as.numeric(y.orig)
    faceter = eval(m$faceter, data, parent.frame())
    if (is.null(faceter)) faceter = 1

    xr = rng(as.numeric(x))
    xincr = (if (xr == 0) 1 else xr) / 50 * length(unique(faceter))
    df = if (discrete)
        dodge.offsets.discrete(x, y, faceter, 5, xincr, rng(y)/100) else
        dodge.offsets.continuous(x, y, faceter, xincr, rng(y)/100)
    p = qplot(x, y, data = df, geom = "blank") +
        geom_point(aes(
            x = as.numeric(x) + xoff,
            y = if (discrete) y + yoff else y)) +
        xlab(m$x) + ylab(m$y)
    if (length(faceter) > 1) p = p +
        facet_grid(. ~ faceter)
    if (discrete) p = p +
        scale_y_continuous(
            breaks = if (is.factor(y.orig))
                     seq_along(levels(y.orig))
                else min(y) : max(y),
            labels = if (is.factor(y.orig))
                     levels(y.orig)
                else min(y) : max(y)) +
        no.gridlines()
    if (xr == 0) p = p +
        xlab("") + scale_x_discrete(breaks = NULL)
    p}

dodge.offsets.discrete = function(x, y, faceter, stack.width, xincr, yincr)
    plyr::ddply(data.frame(x, faceter, y), plyr::.(interaction(x, faceter), y), function(df) data.frame(
        x = df$x,
        faceter = df$faceter,
        xoff = (sa0(df$x) %% stack.width - stack.width / 2)*xincr,
        yoff = center(sapply(sa0(df$x), function(n)
            floor(n / stack.width) * yincr))))

dodge.offsets.continuous = function(x, y, faceter, xincr, ytol)
    plyr::ddply(data.frame(x, faceter, y), plyr::.(interaction(x, faceter)), function(df)
       {df = ordf(df, y)
        xoff = rep(0, length(df$y))
        for (i in seq_along(df$y)[-1])
           {before = df$y[1 : i - 1]
            near = which(df$y[i] - before <= ytol)
            xo = 0
            xoff[i] = callCC( function(done) repeat
              # Find the best xoff that isn't used by any of the
              # near values.
               {for (x in sample(c(-xo, xo)))
                    if (all(xoff[near] != x))
                        done(x)
                xo = xo + xincr})}
        cbind(df, x = df$x, faceter = df$faceter, xoff)})

# --------------------------------------------------
# MCMC
# --------------------------------------------------

as.bugs.model = function(expr, ...)
# So you can put BUGS models in your source code without quoting
# them. Extra arguments are named substitutions:
#   as.bugs.model({y <- FOO + 5}, FOO = 10)
#   => "model {\n    y <- 10 + 5\n}"
# `splice.into.expr` is used, so you can substitute in multiple
# statements like so:
#   as.bugs.model({f(); FOO; g();}, FOO = quote({h(); i();}))
#   => "model {\n    f()\n    h()\n    i()\n    g()\n}"
  {m = match.call()
   "model " %.% paste(collapse = "\n", deparse(
       splice.into.expr(m$expr, list(...))))}

std.jags.rngs = c("base::Wichmann-Hill", "base::Marsaglia-Multicarry", "base::Super-Duper", "base::Mersenne-Twister")
multijags = function(
        model.code,
        data, inits, monitor,
        n.adapt = 1000, n.nonadapt = 0,
        n.sample = 1000 * thin, thin = 1,
        jags.sample.monitor = character(), jags.sample.n = 250,
        parallel = T, quiet = F,
        bypass.cache = F, cache.key = punl(model.code, n.adapt, n.nonadapt, n.sample))
   {if (!is.list(inits) || !is.list(inits[[1]]))
        stop("inits is not a list of lists")
    if (!is.character(model.code) || length(model.code) != 1)
        stop("model.code is not a string")

    cache.dirs = qw(Kodi, multijags)
    if (!bypass.cache)
       {result = loadCache(cache.key, dirs = cache.dirs)
        if (!is.null(result))
            return(result)}

    n.chains = length(inits)
    load.module("lecuyer")
    # We make a temporary file instead of using a text connection
    # because jags.model sometimes handles text connections
    # badly, for some reason. (It may have to do with the
    # parallelism.)
    model.file = tempfile()
    writeLines(model.code, model.file)

    result = tryCatch(finally = unlink(model.file),
       {if (parallel)
           {results = foreach::foreach(i = 1 : n.chains) %dopar%
               # We show progress bars only for chain 1.
               {inits = inits[[i]]
                #inits$.RNG.name = std.jags.rngs[[(i %/% length(std.jags.rngs)) + 1]]
                inits$.RNG.name = "lecuyer::RngStream"
                if (!(".RNG.seed" %in% names(inits)))
                    inits$.RNG.seed = floor(runif(1, 0, 1e9))
                jags = jags.model(model.file, data, inits, n.chains = 1,
                    n.adapt = 0, quiet = i != 1 | quiet)
                adapt(jags, n.iter = n.adapt, end.adaptation = T,
                    progress.bar = ifelse(i == 1, "text", "none"))
                if (n.nonadapt)
                   update(jags, n.iter = n.nonadapt,
                       progress.bar = ifelse(i == 1, "text", "none"))
                samp = coda.samples(jags, variable.names = monitor,
                    n.iter = n.sample, thin = thin,
                    progress.bar = ifelse(i == 1, "text", "none"))
                jsamp = list()
                if (length(jags.sample.monitor))
                    jsamp = jags.samples(jags,
                        variable.names = jags.sample.monitor,
                        n.iter = jags.sample.n)
                list(jags, samp[[1]], jsamp)}
            list(
                #jags = lapply(results, function(l) l[[1]]),
                data = data, 
                samp = do.call(mcmc.list, lapply(results, function(l) l[[2]])),
                jsamp = `names<-`(
                    lapply(jags.sample.monitor, function(vname)
                        `dimnames<-`(
                            do.call(abind::abind, lapply(results,
                                function(l) drop( l[[3]][[vname]] ))),
                            NULL)),
                    jags.sample.monitor))}
        else
           {if (length(jags.sample.monitor))
                stop("multijags: jags.sample.monitor with !parallel not implemented")
            for (i in 1 : length(inits))
               {inits[[i]]$.RNG.name = "lecuyer::RngStream"
                if (!(".RNG.seed" %in% names(inits[[i]])))
                    inits[[i]]$.RNG.seed = floor(runif(1, 0, 1e9))}
            jags = jags.model(model.file, data, inits,
                n.chains, n.adapt = 0, quiet)
            adapt(jags, n.iter = n.adapt, end.adaptation = T,
                progress.bar = ifelse(quiet, "none", "text"))
            if (n.nonadapt)
                update(jags, n.iter = n.nonadapt,
                   progress.bar = ifelse(quiet, "none", "text"))
            samp = coda.samples(jags, variable.names = monitor,
                n.iter = n.sample, thin = thin,
                progress.bar = ifelse(quiet, "none", "text"))
            list(data = data, samp = samp)}})
    saveCache(result, cache.key, dirs = cache.dirs)
    result}

coda.vars = function(samp, str)
# 'coda.vars(samp, "rho")' gets all the parameter names in the
# mcmc.list 'samp' that begin with "rho[", as well as any single
# parameter named "rho".
    varnames(samp)[
        varnames(samp) %in% str |
        substr(varnames(samp), 1, nchar(str) + 1) %in%
            paste(str, "[", sep = "")]

coda.check = function(samp, vstr, actual = NULL, sorted = F, xlab = NULL)
# Do 'coda.check(samp, "rho", rho)', where 'rho' has the correct
# parameter values, to plot predicted values of the parameters
# against the true values. Coverage information is also printed.
   {vars = if (length(vstr) == 1) coda.vars(samp, vstr) else vstr
    d = as.data.frame(t(mapcols(
        as.matrix(samp[, vars ]),
        function(v) quantile(v, c(.025, .25, .5, .75, .975)))))
    names(d) = qw(lo, midlo, mid, midhi, hi)
    if (!is.null(actual))
       {d$actual = actual
        with(d, message(sprintf("The 95%% posterior intervals have %d%% coverage (%d of %d)",
            round(100 * mean(actual >= lo & actual <= hi)),
            sum(actual >= lo & actual <= hi),
            nrow(d))))
        with(d, message(sprintf("The 50%% posterior intervals have %d%% coverage (%d of %d)",
            round(100 * mean(actual >= midlo & actual <= midhi)),
            sum(actual >= midlo & actual <= midhi),
            nrow(d))))}
    d$obs =
       (if (sorted)
            rank((if (is.null(actual)) d$mid else actual), ties.method = "first")
        else if (!is.null(xlab))
            factor(1 : nrow(d), labels = xlab)
        else if (length(vstr) > 1)
            factor(vstr, levels = vstr)
        else
            1 : nrow(d))
    ggplot(data = d) +
        geom_crossbar(aes(x = obs, y = mid, ymin = midlo, ymax = midhi, group = obs),
               color = NA, fill = "blue", alpha = .1) +
        geom_crossbar(aes(x = obs, y = mid, ymin = lo, ymax = hi, group = obs)) +
        (if (is.null(actual)) geom_blank() else geom_point(aes(x = obs, y = actual),
            color = "red")) +
        ylab("predicted") + xlab("observation") +
        no.gridlines()}

standardize.stan.code = function(stan.code)
# Remove comments and reduce whitespace.
    gsub("\\s+", " ",
    sub("\\s+$", "",
    sub("^\\s+", "",
    gsub("/\\*.+?\\*/", "",
    gsub("//.*?(\n|$)", "\\1",
    stan.code)))))

cached_stan_model = function(code, bypass.cache = F)
   {library(rstan)
    dirs = c("Kodi", "stan", "models")
    key = list(code = standardize.stan.code(code))
    if (!bypass.cache)
       {model = loadCache(key, dirs = dirs)
        if (!is.null(model))
            return(model)}
    model = stan_model(model_code = code)
    saveCache(model, key, dirs = dirs)
    model}

stan.traceplot = function(fit, parameter)
    qplot(data = reshape2::melt(drop(rstan::extract(fit, parameter, inc_warmup = T))),
        x = iterations * fit@stan_args[[1]]$thin,
        y = value,
        color = chains,
        geom = "line") +
    scale_color_discrete(guide = F) +
    geom_vline(x = fit@stan_args[[1]]$warmup) +
    scale_x_continuous(name = "iteration", expand = c(0, 0)) +
    scale_y_continuous(name = parameter, expand = c(0, 0))

plot.demonoid.ppc = function(pred)
# For LaplacesDemon demonoid.ppc objects.
   {yhat.summary = as.data.frame(t(maprows(pred$yhat,
        function(v) quantile(v, c(.025, .5, .975)))))
    names(yhat.summary) = qw(lo, mid, hi)
    yhat.summary$obs = 1 : nrow(yhat.summary) #rank(pred$y, ties.method = "first")
    ys = data.frame(obs = 1 : length(pred$y), y = pred$y)
    ggplot() +
        geom_crossbar(data = yhat.summary, aes(
            x = obs, y = mid, ymin = lo, ymax = hi, group = obs)) +
        geom_point(data = ys, aes(obs, y),
            color = "red") +
        ylab("y") + xlab("observation") +
        no.gridlines()}

# --------------------------------------------------
# Tversky   <https://github.com/Kodiologist/Tversky>
# --------------------------------------------------

tversky.sid = function(sn)
    sprintf("s%04d", sn)

unpack.tversky = function(db.path,
        include.incomplete = T, exclude.sns = numeric())
   {library(DBI)
    library(RSQLite)
    library(reshape2)

    db = dbConnect(dbDriver("SQLite"), dbname = db.path)

    subjects = dbGetQuery(db, '
      select Subjects.*,
          cast(began_t as float) as began_t,
          MTurk.hitid as hit, MTurk.assignmentid as asgmt, Mturk.workerid as worker
      from Subjects
          left join
              (select sn, min(first_sent) as began_t from Timing group by sn)
              using (sn)
          left join MTurk using (sn)')

    subjects = transform(subjects,
        task = factor(task, ordered = T, levels =
            unique(ordf(subjects, began_t)$task)))
      # This ensures that the levels of 'task' are in
      # chronological order.

    subjects = subset(subjects, !(sn %in% exclude.sns) &
        (include.incomplete | !is.na(completed_t)))
    subjects = cbind(
        s = ordered(tversky.sid(subjects$sn)),
        subjects)
    row.names(subjects) = subjects$s
    subjects = transform(subjects,
        tv = NA,
        experimenter = factor(experimenter),
        ip = factor(ip),
        hit = factor(hit),
        asgmt = factor(asgmt),
        worker = factor(worker),
        consented_t = posix.ct(as.numeric(
            ifelse(consented_t == "assumed", NA, consented_t))),
        began_t = posix.ct(began_t),
        completed_t = posix.ct(as.numeric(completed_t)))
    subjects = transform(subjects,
        tv = as.integer(task))
          # "tv" for "task version"

    dlong = ordf(
        transform(subset(dbReadTable(db, "D"), sn %in% subjects$sn),
            k = factor(k)),
        sn)
    dlong = cbind(
        s = ordered(tversky.sid(dlong$sn)),
        subset(dlong, select = -sn))
    dwide = ordf(dcast(dlong, s ~ k, value.var = "v"), s)
    row.names(dwide) = dwide$s

    timing = ordf(
        transform(
            subset(dbReadTable(db, "Timing"), sn %in% subjects$sn),
            k = factor(k),
            d = received - first_sent),
        sn, first_sent)
    timing = cbind(
        s = ordered(tversky.sid(timing$sn)),
        subset(timing, select = -sn))
    tdiff = dcast(timing, s ~ k, value.var = "d")
    row.names(tdiff) = as.character(tdiff$s)

    subjects = subjects[c(
        "s", "experimenter", "ip",
        "hit", "asgmt", "worker",
        "task", "tv",
        "consented_t", "began_t", "completed_t")]
    list(
        subjects = subjects,
        dlong = dlong,
        dwide = dwide,
        timing = timing,
        tdiff = tdiff)}

# --------------------------------------------------
# ksource
# --------------------------------------------------

ksource.envs = `names<-`(list(), character())

ksource = function(expr, text, file, 
        ename = (if (missing(file)) "generic" else dirname(file)))
# Like 'source' and 'eval' but with better containment.
# '.GlobalEnv' is hidden, and only packages loaded with 'library'
# while inside the local environment are visible. This is
# implemented by managing a separate search path for the local
# environment.
#
# Only one of 'expr', 'text', or 'file' should be provided.
   {ename = paste0("ksrc:", ename)
    if (ename %in% names(ksource.envs))
       {e = ksource.envs[[ename]]
        e$library = ksource.library(e)}
    else
       {e = new.env(parent = baseenv())
        attr(e, "name") = ename
        # Shadow "library" to add to e's private search path
        # in addition to the real search path.
        e$library = ksource.library(e)
        # Add some default packages.
        e$library(stats)
        e$library(utils)
        e$library(graphics)
        e$library(grDevices)}

    if (sum(c(!missing(file), !missing(expr), !missing(text))) != 1)
        stop("Exactly one must be provided among file, expr, text")
    if (!missing(file))
        v = source(file, local = e)
    else if (!missing(expr))
        v = eval(expr, envir = e)
    else # !missing(text)
        v = eval(parse(text = text), envir = e)

    if (!(ename %in% names(ksource.envs)))
       {ksource.envs[[ename]] <<- e}
    if (ename %in% search())
       detach(ename, character.only = T)
    # Remove 'library' before attaching so it isn't shadowed
    # for the user.
    rm("library", envir = e)
    attach(e, name = ename)

    v}

ksource.library = function(e) function(libname)
   {m = match.call()
    libname = as.character(m$libname)
    base::library(libname, character.only = T)
    # If this library isn't in e's private search path, insert it.
    # We don't include "Depends" packages. This can be construed
    # as a feature insofar as it requires the user to explicitly
    # request all required packages.
    pkgname = paste0("package:", libname)
    if (!(pkgname %in% env.chain(e)))
       {pkg = copy.env(as.environment(pkgname),
            parent = parent.env(e))
        parent.env(e) = pkg}
    T}

# --------------------------------------------------
# Org
# --------------------------------------------------

org.write.table = function(x, na = "NA", ...)
   {if (is.table(x) && length(dim(x)) == 1)
      {x = unclass(x)
       dimnames(x)[[1]][ is.na(dimnames(x)[[1]]) ] = "NA"
       utils::write.table(data.frame(count = x), na = "", ...)}
    else if (inherits(x, "gelman.diag"))
      {x = round(digits = 2, x$psrf[, "Upper C.I.", drop = F])
       dimnames(x)[[1]] = sprintf("`%s`", dimnames(x)[[1]])
       utils::write.table(format(x, drop0trailing = F), na = "", ...)}
    else if ((is.numeric(x) || is.factor(x)) && (is.null(dim(x)) || length(dim(x)) == 1))
       utils::write.table(
           if (is.null(names(x)))
              data.frame(row.names = "",
                  value = paste(x, collapse = " "))
           else
              data.frame(value = x),
           na = na,
           ...)
    else if (inherits(x, "data.frame") || inherits(x, "matrix"))
       utils::write.table(format(x, drop0trailing = F), na = "", ...)
    else
       utils::write.table(x, na = "", ...)}

default.cache.dirs = c("Kodi", "adhoc")

cache = function(cache.key, v, cache.dirs = default.cache.dirs, comment = NULL)
   {cached.v = loadCache(cache.key, dirs = cache.dirs)
    if (is.null(cached.v))
       {saveCache.default(v, cache.key, dirs = cache.dirs,
            comment = comment)
        v}
    else
       cached.v}

cached = function(cache.dirs = default.cache.dirs) function(v)
   {m = match.call()
    expr.str = paste(deparse(m$v), collapse = " ")
    cache(list(expr = expr.str), v, cache.dirs,
        comment = paste("expr:", expr.str))}

kodi.eval = function(expr, ename)
   ksource(ename = ename, expr = substitute(
        with(withVisible(expr),
        if (visible) value else "")))
