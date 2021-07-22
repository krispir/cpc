# This file contains all generics in alphabetical order

# A ############################################################################
# B ############################################################################
# C ############################################################################

setGeneric("calculatePeakCharacteristics", function(x) standardGeneric("calculatePeakCharacteristics"))
setGeneric("checkPeaksAgainstCriteria", function(x) standardGeneric("checkPeaksAgainstCriteria"))
setGeneric("cpt", function(x) standardGeneric("cpt"))
setGeneric("cpt<-", function(x, value) standardGeneric("cpt<-"))

# D ############################################################################
setGeneric("determineMaxSigma", function(x, scantime, scanrate) standardGeneric("determineMaxSigma"))
setGeneric("determineFilterOutcomes", function(x) standardGeneric("determineFilterOutcomes"))

# E ############################################################################
# F ############################################################################

setGeneric("filePaths", function(x) standardGeneric("filePaths"))
setGeneric("filterPeaks", function(x) standardGeneric("filterPeaks"))
setGeneric("filteredObject", function(x) standardGeneric("filteredObject"))
setGeneric("fitEMG", function(x) standardGeneric("fitEMG"))

# G ############################################################################

setGeneric("getChromatogram", function(x, id) standardGeneric("getChromatogram"))
setGeneric("getFilteredXCMS", function(x) standardGeneric("getFilteredXCMS"))
setGeneric("getMzRange", function(x) standardGeneric("getMzRange"))
setGeneric("getOriginalXCMS", function(x) standardGeneric("getOriginalXCMS"))
setGeneric("getPeaklist", function(x) standardGeneric("getPeaklist"))
setGeneric("getProcData", function(x, value = NULL) standardGeneric("getProcData"))
setGeneric("getProcParams", function(x, value = NULL) standardGeneric("getProcParams"))
setGeneric("getParam", function(x, param) standardGeneric("getParam"))
setGeneric("getRetainedPeaks", function(x) standardGeneric("getRetainedPeaks"))
setGeneric("getRemovedPeaks", function(x) standardGeneric("getRemovedPeaks"))
setGeneric("getResults", function(x) standardGeneric("getResults"))
setGeneric("getXIC", function(x, mzrange, scanrange = NULL, method = 1) standardGeneric("getXIC"))
setGeneric("getFilterOutcomes", function(x) standardGeneric("getFilterOutcomes"))


# H ############################################################################

setGeneric("hasPeakTable", function(x) standardGeneric("hasPeakTable"))
setGeneric("hasCharacterizedPeakTable", 
           function(x) standardGeneric("hasCharacterizedPeakTable"))

# I ############################################################################
# J ############################################################################
# K ############################################################################
# L ############################################################################
# M ############################################################################
# N ############################################################################
# O ############################################################################
# P ############################################################################

setGeneric("parsePeaklist", function(x) standardGeneric("parsePeaklist"))
setGeneric("parseFeatures", function(x) standardGeneric("parseFeatures"))
setGeneric("parseMz", function(x) standardGeneric("parseMz"))
setGeneric("peaksToKeep", function(x, returnBoolean = FALSE) standardGeneric("peaksToKeep"))
setGeneric("plotPeak", function(x, plotEMG = F, plotXCMS = T, annotation = character(1)) standardGeneric("plotPeak"))
setGeneric("processChromatogram", function(x) standardGeneric("processChromatogram"))
setGeneric("processPeaks", function(x) standardGeneric("processPeaks"))

# Q ############################################################################
# R ############################################################################
# S ############################################################################

setGeneric("scantime", function(x) standardGeneric("scantime"))
setGeneric("setPeaklist<-", function(x, value) standardGeneric("setPeaklist<-"))
setGeneric("setProcData<-", function(x, value) standardGeneric("setProcData<-"))
setGeneric("setProcParams<-", function(x, value) standardGeneric("setProcParams<-"))
setGeneric("setParam<-", function(x, value) standardGeneric("setParam<-"))
setGeneric("setResults<-", function(x, value) standardGeneric("setResults<-"))
setGeneric("setFilterOutcomes<-", function(x, value) standardGeneric("setFilterOutcomes<-"))
setGeneric("setXIC<-", function(x, value) standardGeneric("setXIC<-"))
setGeneric("show", function(x) standardGeneric("show"))
setGeneric("smoothChromatogram", function(x) standardGeneric("smoothChromatogram"))
setGeneric("summary", function(x) standardGeneric("summary"))

# T ############################################################################
# U ############################################################################
# V ############################################################################
# X ############################################################################

setGeneric("xdObj", function(x) standardGeneric("xdObj"))
setGeneric("xdObj<-", function(x, value) standardGeneric("xdObj<-"))

# Y ############################################################################
# Z ############################################################################