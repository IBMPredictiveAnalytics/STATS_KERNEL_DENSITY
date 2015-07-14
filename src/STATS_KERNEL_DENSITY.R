#Licensed Materials - Property of IBM
#IBM SPSS Products: Statistics General
#(c) Copyright IBM Corp. Year1, 2011, 2014
#US Government Users Restricted Rights - Use, duplication or disclosure 
#restricted by GSA ADP Schedule Contract with IBM Corp.


# Version: 1.0.2
# Author: JKP, IBM SPSS

# history
# 03-Mar-2012 Initial version
# 26-Jun-2014 Localization changes

helptext="The STATS KERNEL DENSITY procedure produces a dataset of the smoothed
density, a summary table, and an optional plot.

STATS KERNEL DENSITY VARIABLES=varnameList
KERNEL = {EPANECHNIKOV*| GAUSSIAN | RECTANGULAR |
    TRIANGULAR | BIWEIGHT | COSINE | OPTCOSINE}
OUTPUTDS= dsnameList N=integer BANDWIDTHFACTOR = number
[/OPTIONS PLOT={YES*|NO}

All parameters are optional except VARIABLES.

VARIABLES specifies the variables whose distributions will be calculated.

KERNEL specifies the kernel to be used for smoothing.  EPANECHNIKOV is
the default.

BANDWIDTHFACTOR specifies a multiplier for the automatic bandwidth.  The
default is 1.

N specifies at how many points the probability is calculated.  The default is 512.

OUTPUTDS specifies the names for new output datasets to hold the calculated
values.  If specified, there must be as many dataset names as variables.  The
dataset names must not already be in use. There will be N cases in each dataset.

PLOT specifies whether or not to plot the density function.

Missing values are automatically discarded.

If any new datasets are created, and the active dataset is unnamed,
it will be closed automatically as per standard rules.
"

fitdist<-function(variables, outputds=NULL, n=512, kernel="epanechnikov", adjust=1., doplot=TRUE) {
    
    setuplocalization("STATS_KERNEL_DENSITY")
    # variables loop
    if (length(variables) != length(outputds) && !is.null(outputds)) {
        warns$warn(gtxt("There must be the same number of output dataset names as variables"),
            dostop=TRUE)
    }
    wtvar = spssdictionary.GetWeightVariable()
    augvars = variables
    nvar = length(variables)
    if (!is.null(wtvar)) {
        augvars[nvar+1] = wtvar
    }
    
    dta<-spssdata.GetDataFromSPSS(augvars,missingValueToNA=TRUE)
    if (!is.null(wtvar)) {
        wt = dta[,nvar+1]/sum(dta[, nvar+1])
    }
    else {
    wt = NULL
    }
    i = 1
    resdata = list()
    bwlist = ""
    StartProcedure("Smooth Distribution Fit","STATSKERNELSMOOTH") 

    for (var in variables) {
        res = tryCatch(density(dta[, i], adjust=adjust, kernel = kernel, n=n, na.rm=TRUE, weights=wt),
            error=function(e) {
            warns$warn(gtxtf("The distribution could not be fit.  Variable: %s, Kernel: %s", var, kernel),
                dostop=TRUE)}
            )
            
        # accumulate distribution parameters
        fitsum = summary(res$x)
        if (i == 1) {
            ressum = fitsum
        }
        else {
            ressum = rbind(ressum, fitsum) 
        }
        resdata[[i]] = data.frame(cbind(res$x, res$y))
        bwlist = paste(bwlist, sprintf("%.4f", res$bw), sep="  ")
        if (doplot) {
            plot(res, main = gtxtf("Density\nVariable: %s, Kernel = %s", var, kernel),
                col="blue", lwd=3)
        }
    i = i+1
    }

    # summary pivot table - statistics are columns
    ressum = data.frame(rbind(ressum))
    names(ressum) = c(gtxt("Minimum"), gtxt("Percentile 25"), gtxt("Median"), 
        gtxt("Mean"), gtxt("Percentile 75"), gtxt("Maximum"))
    row.names(ressum) = variables
    spsspivottable.Display(ressum, title = gtxt("Distribution Statistics"),
        template="DISTSTATS",
        caption= gtxtf("Kernel Smooth: %s, Number of cases: %s, Bandwidth: %s", kernel, res$n, bwlist),
        outline = gtxt("Distribution Statistics"), 
        rowdim = gtxt("Variables"),
        coldim = gtxt("Statistics")
    )
    spsspkg.EndProcedure()
    if (!is.null(outputds)) {
        i = 1
        for (var in variables) {
            outputdsi = outputds[[i]]
            if (var == "Probability") {
                pname = "Probability_1"
            }
            else {
                pname = "Probability"
            }
            vdict = spssdictionary.CreateSPSSDictionary(c(var, "", 0, "F8.2", "scale"), c(pname, "", 0, "F8.4", "scale"))
            tryCatch({
                spssdictionary.SetDictionaryToSPSS(outputdsi, vdict)
                spssdata.SetDataToSPSS(outputdsi, resdata[[i]])},
                error = function(e) {warns$warn(gtxtf("Could not create dataset %s.  It may already exist", outputdsi),
                    dostop=TRUE)}
            )
            i = i+1
        }
    spssdictionary.EndDataStep()
    }

    # clean up workspace
    res <- tryCatch(rm(list=ls()),warning=function(e){return(NULL)})
}

# override for api to account for extra parameter in V19 and beyond
StartProcedure<-function(procname, omsid) {
    if (substr(spsspkg.GetSPSSVersion(),1, 2) >= 19) {
       spsspkg.StartProcedure(procname,omsid)
       }
    else {
       spsspkg.StartProcedure(omsid)
       }
}

Warn = function(procname, omsid) {
    # constructor (sort of) for message management
    lcl = list(
        procname=procname,
        omsid=omsid,
        msglist = list(),  # accumulate messages
        msgnum = 0
    )
    # This line is the key to this approach
    lcl = list2env(lcl) # makes this list into an environment
    
    lcl$warn = function(msg=NULL, dostop=FALSE, inproc=FALSE) {
        # Accumulate messages and, if dostop or no message, display all
        # messages and end procedure state
        # If dostop, issue a stop.
        
        if (!is.null(msg)) { # accumulate message
            assign("msgnum", lcl$msgnum + 1, envir=lcl)
            # There seems to be no way to update an object, only replace it
            m = lcl$msglist
            m[[lcl$msgnum]] = msg
            assign("msglist", m, envir=lcl)
        } 
        
        if (is.null(msg) || dostop) {
            lcl$display(inproc)  # display messages and end procedure state
            if (dostop) {
                stop(gtxt("End of procedure"), call.=FALSE)  # may result in dangling error text
            }
        }
    }
    
    lcl$display = function(inproc=FALSE) {
        # display any accumulated messages as a warnings table or as prints
        # and end procedure state, if any
        
        if (lcl$msgnum == 0) {   # nothing to display
            if (inproc) {
                spsspkg.EndProcedure()
            }
        } else {
            if (!inproc) {
                procok =tryCatch({
                    StartProcedure(lcl$procname, lcl$omsid)
                    TRUE
                },
                error = function(e) {
                    FALSE
                }
                )
            }
            if (procok) {  # build and display a Warnings table if we can
                table = spss.BasePivotTable("Warnings ","Warnings") # do not translate this
                rowdim = BasePivotTable.Append(table,Dimension.Place.row, 
                                               gtxt("Message Number"), hideName = FALSE,hideLabels = FALSE)
                
                for (i in 1:lcl$msgnum) {
                    rowcategory = spss.CellText.String(as.character(i))
                    BasePivotTable.SetCategories(table,rowdim,rowcategory)
                    BasePivotTable.SetCellValue(table,rowcategory, 
                                                spss.CellText.String(lcl$msglist[[i]]))
                }
                spsspkg.EndProcedure()   # implies display
            } else { # can't produce a table
                for (i in 1:lcl$msgnum) {
                    print(lcl$msglist[[i]])
                }
            }
        }
    }
    return(lcl)
}

gtxt <- function(...) {
    return(gettext(...,domain="STATS_KERNEL_DENSITY"))
}

gtxtf <- function(...) {
    return(gettextf(...,domain="STATS_KERNEL_DENSITY"))
}

setuplocalization = function(domain) {
    # find and bind translation file names
    # domain is the root name of the extension command .R file, e.g., "SPSSINC_BREUSCH_PAGAN"
    # This would be bound to root location/SPSSINC_BREUSCH_PAGAN/lang

    fpath = Find(file.exists, file.path(.libPaths(), paste(domain, ".R", sep="")))
    bindtextdomain(domain, file.path(dirname(fpath), domain, "lang"))
} 

Run<-function(args){
    
    cmdname = args[[1]]
    args <- args[[2]]
    oobj<-spsspkg.Syntax(templ=list(
            spsspkg.Template("VARIABLES", subc="",  ktype="existingvarlist", var="variables", islist=TRUE),
            spsspkg.Template("KERNEL", subc="",  ktype="str", var="kernel", islist=FALSE,
            vallist = list("epanechnikov","gaussian", "rectangular", "triangular", "biweight",
                "cosine", "optcosine")),
            spsspkg.Template("OUTPUTDS", subc="", ktype="varname", var="outputds", islist=TRUE),
            spsspkg.Template("N", subc="", ktype="int", var="n"),
            spsspkg.Template("BANDWIDTHMULT", subc="", ktype="float", var="adjust"),
            spsspkg.Template("PLOT", subc="OPTIONS", ktype="bool", var="doplot")
            ))
                
    if ("HELP" %in% attr(args,"names"))
        #writeLines(helptext)
        helper(cmdname)
    else
        res <- spsspkg.processcmd(oobj,args,"fitdist")
}

helper = function(cmdname) {
    # find the html help file and display in the default browser
    # cmdname may have blanks that need to be converted to _ to match the file
    
    fn = gsub(" ", "_", cmdname, fixed=TRUE)
    thefile = Find(file.exists, file.path(.libPaths(), fn, "markdown.html"))
    if (is.null(thefile)) {
        print("Help file not found")
    } else {
        browseURL(paste("file://", thefile, sep=""))
    }
}
if (exists("spsspkg.helper")) {
assign("helper", spsspkg.helper)
}
