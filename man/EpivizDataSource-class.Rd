% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EpivizDataSource-class.R
\docType{class}
\name{EpivizDataSource-class}
\alias{EpivizDataSource-class}
\alias{EpivizDataSource}
\title{Data container for an Epiviz Data Source component.}
\description{
Data container for an Epiviz Data Source component.
}
\section{Fields}{

\describe{
\item{\code{name}}{(character) Epiviz Data Source (tag name).}

\item{\code{class}}{(CharacterOrNULL) Epiviz chart's class attribute.}

\item{\code{id}}{(character) Epiviz chart's id attribute.}

\item{\code{provider_type}}{(character)}

\item{\code{provider_id}}{(character)}

\item{\code{provider_url}}{(character)}
}}

\section{Methods}{

\describe{
\item{\code{get_attributes()}}{Get attributes for rendering chart}

\item{\code{get_class()}}{Get class}

\item{\code{get_id()}}{Get id}

\item{\code{get_name()}}{Get name}

\item{\code{get_provider_id()}}{Get provider id}

\item{\code{get_provider_type()}}{Get provider type}

\item{\code{get_provider_url()}}{Get provider url}

\item{\code{render_component()}}{Render to html}

\item{\code{set_class(class)}}{Set chart class}

\item{\code{set_id(id)}}{Set chart id}

\item{\code{set_name(name)}}{Set name}

\item{\code{set_provder_url(url)}}{Set provider url}

\item{\code{set_provider_id(id)}}{Set provider id}

\item{\code{set_provider_type(type)}}{Set provider type}
}}
