% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat1004.R
\docType{package}
\name{stat1004}
\alias{stat1004}
\alias{stat0002}
\alias{stat1004-package}
\title{stat0002 (stat1004): Introduction to Probability and Statistics at UCL}
\description{
Provides example datasets, R code and tutorials to help students taking
STAT0002 (formerly STAT1004) Introduction to Probability and Statistics at
University College London to understand the course material and to see how
R can be used to perform some of the analyses in the course.
This package can also be used to complement STAT0004 Introduction to
Practical Statistics, enabling students to become more familiar with R
code by seeing it in action and playing with it themselves.
}
\section{Please note}{

\strong{The main sources of information about the material in STAT0002
are the lectures and tutorials, with notes, lecture slides, exercises
etc available from the
\href{https://moodle.ucl.ac.uk/course/view.php?id=8579&section=0}{STAT0002 Moodle page}.
The focus of this R package is mainly on R code.}
}

\section{How to learn about R}{

The best way to learn how to use a computer program is to play with code and
see what it does.  R is based on \emph{functions} that look like \code{name()}, where
\code{name} is the name of the function and where various \emph{arguments} are inserted
inside the brackets \code{()}.  In the R code provided with this package you will
see many functions like this.  To find out what a function called \code{name} does
use either \code{?name} or \code{help(name)}.

If you have any questions about this package please ask them via the
\href{https://moodle.ucl.ac.uk/mod/hsuforum/view.php?id=755813}{STAT0002 Moodle Discussion Forum}.
}

\section{Vignettes (tutorials demonstrating how to use R code)}{


\itemize{
  \item {\href{../doc/stat1004-ch1a-shuttle-vignette.html}{Chapter 1: Challenger Space Shuttle Disaster}}
  \item {\href{../doc/stat1004-ch1b-stochastic-simulation-vignette.html}{Chapter 1: Stochastic Simulation}}
  \item {\href{../doc/stat1004-ch2a-descriptive-statistics-vignette.html}{Chapter 2: Descriptive Statistics}}
  \item {\href{../doc/stat1004-ch2b-graphs-vignette.html}{Chapter 2: Graphs (One Variable)}}
  \item {\href{../doc/stat1004-ch2c-graphs-vignette.html}{Chapter 2: Graphs (More Than One Variable)}}
  \item {\href{../doc/stat1004-ch2d-box-cox-transformation-vignette.html}{Chapter 2: Transformation Of Variable}}
}
}

\section{Datasets}{


\itemize{
  \item {\link[=shuttle]{Challenger Space Shuttle Disaster}}
  \item {\link[=days]{Mystery data in Exercises 1}}
  \item {\link[=lymphoma]{T-cell count data in Exercises 2}}
  \item {\link[=UCBAdmissions]{Student Admissions at UC Berkeley}}
  \item {\link[=ox_births]{Oxford Birth Times}}
  \item {\link[=blood_types]{Blood Types}}
  \item {\link[=ftse]{FTSE 100 Share Index}}
  \item {\link[=world_bank]{World Bank Development Indicators}}
  \item {\link[=flu]{Influenza data}}
  \item {\link[=USelection]{The 2000 U.S. Presidential Election}}
  \item {\link[=aussie_births]{Australian Birth Times Data}}
  \item {\link[=hubble]{Nebulae data from Hubble (1929)}}
}
}

\section{\link[=movies]{Movies}}{


Some of the following movies, and other movies are also available in a more
user-friendly form, via the \code{\link[smovie]{smovie}} package.
If you have \code{smovie} installed (\code{install.packages("smovie")})
then you can access these using \code{library(smovie)} and then
\code{movies()}.
See \code{\link[smovie]{movies}} for more details.

\itemize{
  \item {\link[=shuttle_movie]{Challenger Space Shuttle Disaster}}
  \item {\link[=scatterplot_movie]{US election: straightening scatter
    plots}}
  \item {\link[=world_bank_movie]{World Bank Development Indicators}}
  \item {\link[=ox_births_movie]{Oxford Birth Times Simulation}}
  \item {\link[=binomial_pmf_movie]{Binomial p.m.f.}}
  \item {\link[=poisson_process_movie]{Poisson process}}
  \item {\link[=poisson_process_check]{Graphical checking of Poisson process properties}}
  \item {\link[=normal_pdf_movie]{Normal probability density function}}
  \item {\link[=normal_areas_movie]{Normal central probability areas}}
  \item {\link[=qq_plot_movie]{Construction of a (normal) QQ plot}}
  \item {\link[=normal_sampling_distns_movie]{Normal sampling distributions}}
  \item {\link[=clt_normal_movie]{Central Limit Theorem: normal data}}
  \item {\link[=clt_exponential_movie]{Central Limit Theorem: exponential data}}
  \item {\link[=mean_vs_median_normal_movie]{Mean vs median for a normal random sample}}
  \item {\link[=mean_vs_median_t_movie]{Mean vs median for a Student's t random sample}}
  \item {\link[=two_by_two_movie]{Test for lack of association in a 2 by 2 contingency table}}
  \item {\link[=lin_reg_movie]{Simple linear regression using least squares estimation}}
  \item {\link[=lev_inf_movie]{Leverage and influence in simple linear regression}}
  \item {\link[=corr_sim_movie]{Sampling distribution of the correlation coefficient}}
}
}

\seealso{
\code{\link{movies}}: general information about the movies.

The \href{https://moodle.ucl.ac.uk/course/view.php?id=8579&section=0}{STAT0002 Moodle page}
  for: various forms of feedback, a discussion forum, lecturecast recordings,
  course notes, exercises, quizzes, short videos, past exam papers and revision
  lecture material, real investigations.

The \href{https://moodle.ucl.ac.uk/course/view.php?id=8581}{STAT0004 Moodle page}
  (for students taking STAT0004).

The \href{https://moodle.ucl.ac.uk/course/view.php?id=10773}{Introduction to R Moodle page}
  (for students not taking STAT0004, e.g. Natural Sciences students).
}
