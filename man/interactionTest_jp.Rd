% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interactionTest_jp.R
\name{interactionTest_jp}
\alias{interactionTest_jp}
\title{Interaction Test}
\usage{
interactionTest_jp(mean1, se1, mean2, se2, rg = 0)
}
\arguments{
\item{mean1}{effect estimate for strata 1 (e.g. males)}

\item{se1}{standard error for $b_1$}

\item{mean2}{effect estimate for strata 2 (e.g. females)}

\item{se2}{standard error for $b_2$}

\item{rg}{correlation estimate obtain from genome-wide data sets (no LD pruning, no filtering for significant SNPs!), Default: 0 (no correction)}
}
\value{
Data table with input effects and SEs, mean difference ($b_2 - b_1$), mean SE ($\sqrt{SE_1^2 + SE_2^2}$), confidence intervalls, Z-score and Z-score corrected for correlation and their corresponding p-values
}
\description{
This interaction test (also known as difference test) compares the stratum-specific effects, e.g. effect in males and females. $H_0: b_1 = b_2$. Under the assumption of independent samples, unrelated subjects and no latent covariate interacting with $S$ (the stratification variable), the difference test is mathematically equivalent to testing the pooled $G x S$ interaction estimate b_{GxS} obtained from an interaction model. In order to correct for potential correlation between stratum-specific effects $b_1$ and $b_2$, one can include the correlation between $b_1$ and $b_2$, estimated from the two stratum-specific genome-wide data sets (see also LDSCHeritabGenCorr package in our Genstat Group)
}
\details{
interaction test see http://www.bmj.com/content/326/7382/219.long; https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0181038
}
\examples{
\dontrun{
if(interactive()){
 #EXAMPLE1
 }
}
}
\seealso{
\code{\link[data.table]{data.table-package}}
}
