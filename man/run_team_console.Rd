% Generated manually
\name{run_team_console}
\alias{run_team_console}
\title{Run the interactive team coverage console}
\description{
Prompts for 1-6 Pokemon names and prints defensive and offensive type coverage.
}
\usage{
run_team_console(input_fun = readline, analyze_fun = analyze_team, output_fun = cat)
}
\arguments{
  \item{input_fun}{Function used to read input (defaults to \code{readline}).}
  \item{analyze_fun}{Function used to analyze a team (defaults to \code{analyze_team}).}
  \item{output_fun}{Function used to write output (defaults to \code{cat}).}
}
\value{
Invisibly returns \code{NULL} when the user quits.
}
\examples{
\dontrun{
run_team_console()
}
}
