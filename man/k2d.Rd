% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/k2d.r
\name{k2d}
\alias{k2d}
\title{Convert Kernel Matrix to Distance Matrix}
\usage{
k2d(Mat, direction = "k2d", method = "norm", scale = 1, pos = TRUE)
}
\arguments{
\item{Mat}{matrix, either kernel matrix or distance matrix to be converted.}

\item{direction}{a character string "k2d" or "d2k". The latter interpret the Mat as a distance matrix and convert it to a kernel matrix. Default "k2d".}

\item{method}{a character string to specify how the matrix is converted. "Default "norm".}

\item{scale}{a numeric parameter used to scale the matrix. Typically used in exp( - d(x,y)/scale).}

\item{pos}{logical. If \code{TRUE} when \code{direction="d2k"}, negative eigenvalues are round to zero to obtain positive semidefinite kernel matrix. Default \code{TRUE}.}
}
\description{
\code{k2d} provides various methods for converting kernel matrix to distance matrix and vice versa.
}
\details{
There are various ways to convert kernel function values to distance between two points.
Normal-distance (when \code{method="norm"}) means a conversion
d_ND(x,y) = sqrt{ k(x,x) - 2k(x,y) + k(y,y)}.

Cauchy-Schwarz-type conversion (\code{method="CS"}) is more principled way:
d_CS(x,y) = arccos k^2(x,y)/k(x,x)k(y,y).

Other two simple ways are
d_exp(x,y) = exp(- k(x,y)/scale),
which is an exponential-type distance (\code{method="exp"}), and
d_n(x,y) = 1 - k(x,y)/sqrt{ k(x,x)k(y,y)},
which we call naive (\code{method="naive"}).

For converting distance to kernel (\code{direction="d2k"}), it should be noted that we usually have distance between pairs of points only, and distances from "origin" are unknown.
Double-centering (\code{method="DC"}) is the most popular and simple way to convert distance to kernel. However, it does not make positive definite kernel in general,
and it sometimes require post-processing, e.g., cutting off negative eigenvalues (\code{pos=TRUE}).
Another simple way is exponential map (\code{method="exp"}), i.e., k(x,y) = exp( - d(x,y)/scale).
}
\author{
Hideitsu Hino \email{hinohide@cs.tsukuba.ac.jp}, Ken Takano, Yuki Yoshikawa, and Noboru Murata
}

