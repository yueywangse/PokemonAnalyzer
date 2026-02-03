% Generated manually
\name{call_gemini}
\alias{call_gemini}
\title{Call the Gemini API}
\description{
Send a text prompt to a Gemini model and return the first candidate's text.
}
\usage{
call_gemini(prompt, apikey, model = "gemini-2.5-flash")
}
\arguments{
  \item{prompt}{Character string prompt to send.}
  \item{apikey}{Character string Gemini API key.}
  \item{model}{Model name to call (default: "gemini-2.5-flash").}
}
\value{
Character string containing the model output.
}
\examples{
\dontrun{
call_gemini("Explain type advantages in Pokemon battles.", Sys.getenv("GEMINI_API_KEY"))
}
}
