#################################################################
#               General Functions
#
# General functions use in the proyect
#     - data.as.datetime: Convert date column into datetime format
#     - pivot.short.table:
#     - pivot.long.table:
#
# @author Jaimedgp
#################################################################

suppressMessages(library(saqgetr))
suppressMessages(library(lubridate))


data.as.datetime <- function(dataframe, column, FUN){
    # Converte date column into datetime format using lubridate package
    #
    # @params:
    #     dataframe: dataframe with all data
    #     column: name or names of date column
    #     FUN: lubridate function to apply. Can be date ('ymd') or datetime ('ymd <- hms')
    # @return:
    #     dataframe: same dataframe with date column as datetime format

    if (FUN == "ymd_hms") {
        dataframe[, column] <- ymd_hms(dataframe[, column])
    } else if (FUN == "ymd") {
        dataframe[, column] <- ymd(dataframe[, column])
    } else {
        print("No valid FUN. Must be: 'ymd_hms' or 'ymd'")
    }
    dataframe
}


pivot.short.table <- function(df, cols) {
    cmn.nm <- names(df)[-which(names(df) %in% cols)]
    new.df <- data.frame()

    for (cl in cols) {
        new.row <- cbind(df[, cmn.nm],
                         variable=rep(cl, nrow(df)),
                         value=df[, cl])
        new.df <- rbind(new.df,
                        new.row)
    }

    new.df
}


pivot.long.table <- function(df, valueCl, variableCl) {
    cmn.nm <- names(df)[-which(names(df) %in% c(valueCl, variableCl))]
    lv <- levels(as.factor(df[, variableCl]))

    new.df <- cbind(df[df[, variableCl] == lv[1], cmn.nm],
                    df[df[, variableCl] == lv[1], valueCl])
    names(new.df)[ncol(new.df)] <- lv[1]

    if (length(lv) > 1) {
        for (l in lv[2:length(lv)]) {
            new.row <- df[df[, variableCl] == l, ]

            new.df <- merge(new.df,
                            cbind(new.row[, cmn.nm],
                                new.row[, valueCl]),
                            by=cmn.nm, all = T
                            )
            names(new.df)[ncol(new.df)] <- l
        }
    }
    new.df
}
