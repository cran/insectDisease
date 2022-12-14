#' Information on viral pathogen occurrences
#'
#' Host associations for viruses
#' 
#' 
#' \itemize{
#'   \item ERNv. EDWIP record number
#'   \item HostSpecies. Host species
#'   \item VirusType. DNA or RNA virus
#'   \item PathogenSpecies. Viral family 
#'   \item Virus. Virus identity
#'   \item HostStageInfected. Host stage infected
#'   \item HostTissueInfected. Host tissue infected
#'   \item FieldOrLab. Was this a field or lab tested association?
#'   \item Country. What country did the interaction occur in?
#'   \item IntermediateHost. Is there an intermediate host present?
#'   \item CreationDate. Date of initial data entry (wrong)
#'   \item ModificationDate. Modification date of entry (wrong)
#'   \item ProvinceA. Canadian provinces where host-virus interaction occurs
#'   \item PathogenValue. Is there value to the pathogen (can it be used as a control agent?)
#'   \item Group. Viruses.
#'   \item HostTaxID. Host NCBI ID number
#'   \item HostGenus. Host genus
#'   \item HostFamily. Host family
#'   \item HostOrder. Host order
#'   \item HostClass. Host class
#'   \item PathTaxID. Pathogen NCBI ID number
#'   \item PathGenus. Pathogen genus
#'   \item PathFamily. Pathogen family
#'   \item PathOrder. Pathogen order
#'   \item PathClass. Pathogen class
#'   \item PathKingdom. Pathogen kingdom
#'  }
#'
#' @docType data
#' @keywords datasets
#' @name viruses
#' @usage data(viruses)
#' @format A data.frame with 1659 observations 

"viruses"
