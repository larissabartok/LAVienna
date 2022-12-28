##' Visualiserung für Regressionsmodelle
#'
#' Diese Funktion erstellt eine ggplot2-Grafik, in der die Regressionskoeffizienten (standardisiert oder nicht standardisiert) aufgetragen werden.
#' @param lm Objekt der Klasse lm oder glm
#' @param lmz 2. Objekt der Klasse lm oder glm
#' @param standardisieren Sollen die Regressionskoeffizienten standardisiert werden? Default ist TRUE.
#' @param speichern Wo sollen der Plot gespeichert werden? Default ist FALSE, wenn TRUE gesetzt wird, dann ist Pfadangabe notwendig.
#' @param pfad Pfadangabe wo die Grafik gespeichert werden soll. Default ist "U:/Learning Analytics Projekt/09 Grafiken/"
#' @param file Pfad zum File, das Variablenbeschriftungen enthält
#' @param alpha Für Konfidenzintervalle und Signifikanz. Default ist 0.05
#' @param xlim Limits der x-Achse. Default ist c(-0.6, 0.6)
#' @param kasten Soll der Kasten dargestellt werden? Default ist TRUE
#' @param kastencoord x-Koordinaten für den Kasten
#' @param englisch Soll die Grafik Englisch beschriftet werden? Default ist FALSE
#' @param bw Soll die Grafik in Graustufen dargestellt werden? Default ist FALSE
#' @export betaplot
#' @examples betaplot()

betaplot <- function(lm, lmz = NULL, standardisieren = TRUE, speichern = FALSE, pfad = NULL, file = NULL, alpha = 0.05, xlim = c(-0.6, 0.6), kasten = TRUE, kastencoord = c(-0.25, 0.235), englisch = FALSE, bw = FALSE){
  
  # Hier werden Daten aufbereitet, später eigene Hilfsfunktion
  
  z <- qnorm(p= 1- (alpha/2), mean=0, sd=1)
  
  if(bw){
    sig_col <- c("gray30","gray70")
  }else{
    sig_col <- c("cadetblue2","firebrick3")
  }
  
  if(standardisieren){ # wenn logistische Regression dann Odds berechnen später hinzu ---------------
    
    # Das hier produziert blöde Warnmeldung und nicht möglich mit Umlauten in Faktor levels???
    lm2 <- beta.lm(model = lm)
    # lm2 <- reghelper::beta(model = lm)
    
    # FÜR GLM
    if(any(class(lm2) == "summary.glm")){
      lm_data <- as.data.frame(broom::tidy(lm))
      hilfe <- as.data.frame(lm2$coefficients)
      lm_data$estimate <- hilfe$Estimate
      lm_data$std.error <- hilfe$`Std. Error`
      lm_data$statistic <- hilfe$`z value`
      if(is.null(hilfe$`Pr(>|z|)`)){
        lm_data$p.value <- hilfe$`Pr(>|t|)`
      }else{
        lm_data$p.value <- hilfe$`Pr(>|z|)`
      }
    }
    # FÜR LM
    if(any(class(lm2) == "summary.lm")){
      lm_data <- as.data.frame(broom::tidy(lm))
      hilfe <- as.data.frame(lm2$coefficients)
      lm_data$estimate <- hilfe$Estimate
      lm_data$std.error <- hilfe$`Std. Error`
      lm_data$statistic <- hilfe$`z value`
      lm_data$p.value <- hilfe$`Pr(>|t|)`
    }
    lm_data$lower <- lm_data$estimate - z * lm_data$std.error
    lm_data$upper <- lm_data$estimate + z * lm_data$std.error
  } else{
    lm_data <- as.data.frame(broom::tidy(lm))
    lm_data$lower <- lm_data$estimate - z * lm_data$std.error
    lm_data$upper <- lm_data$estimate + z * lm_data$std.error
  }
  
  # Remove intercept xxx hier noch Option mit Intercept in Grafik
  lm_data <- lm_data[-1,]
  
  # Datensatz der Benennung der Variablen angibt
  # Hier gibt es jetzt die Funktion namenplot die das übernimmt
  
  unab <- Reduce(paste, deparse(lm$terms[[3]]))
  unab <- gsub("[[:space:]]", "", unab)
  unab <- as.character(unlist(strsplit(unab, "\\+")))
  
  
  beschriftung_neu <- NULL
  for(i in 1:length(unab)){
    beschriftung <- namenplot(unab[i], lm = lm, englisch = englisch, file = file)
    beschriftung_neu <- c(beschriftung_neu, beschriftung)
  }
  
  position1 <- length(beschriftung_neu)+1
  
  if(is.null(lmz)){
    p <- ggplot() + geom_point(data = lm_data, aes(y=term,x = estimate, color =ifelse((p.value) < alpha,"Ja","Nein")), size = 4) +
      geom_linerange(data = lm_data, aes(xmin = lower, xmax = upper, y = term, color = ifelse((p.value) < alpha,"Ja","Nein")), linetype = 1, size = 1) +
      scale_color_manual("Signifikant", breaks = c("Nein", "Ja"), values= sig_col) +
      theme_grey(base_size = 13)  +
      geom_vline(xintercept = 0, linetype = 2) +
      ylab("Erklärende Variable") +
      theme_minimal(base_size = 13) + xlim(xlim)
    
    if(kasten){p <- p + annotate("label", x = kastencoord, y = position1, label = c("Negativer Effekt", "Positiver Effekt"), size = 4.5, color = c("seagreen","orange2"), fontface=2) +
      scale_y_discrete(limits = rev(c("", lm_data$term)), labels=rev(c("", beschriftung_neu)))}else{
        p <- p + scale_y_discrete(limits = rev(lm_data$term), labels=rev(beschriftung_neu))
      }
    if(standardisieren){
      p <- p + xlab("Standardisierter Regressionskoeffizient")
    }else{p <- p + xlab("Unstandardisierter Regressionskoeffizient") }
  }
  
  if(!is.null(lmz)){ # 2. Modell
    
    if(standardisieren){ # wenn logistische Regression dann Odds berechnen später hinzu ---------------
      
      # Das hier produziert blöde Warnmeldung und nicht möglich mit Umlauten in Faktor levels???
      lm3 <- reghelper::beta(model = lmz)
      
      # FÜR GLM
      if(any(class(lm3) == "summary.glm")){
        lm3_data <- as.data.frame(broom::tidy(lmz))
        hilfe3 <- as.data.frame(lm3$coefficients)
        lm3_data$estimate <- hilfe3$Estimate
        lm3_data$std.error <- hilfe3$`Std. Error`
        lm3_data$statistic <- hilfe3$`z value`
        if(is.null(hilfe3$`Pr(>|z|)`)){
          lm3_data$p.value <- hilfe3$`Pr(>|t|)`
        }else{
          lm3_data$p.value <- hilfe3$`Pr(>|z|)`
        }
      }
      # FÜR LM
      if(any(class(lm3) == "summary.lm")){
        lm3_data <- as.data.frame(broom::tidy(lmz))
        hilfe3 <- as.data.frame(lm3$coefficients)
        lm3_data$estimate <- hilfe3$Estimate
        lm3_data$std.error <- hilfe3$`Std. Error`
        lm3_data$statistic <- hilfe3$`z value`
        lm3_data$p.value <- hilfe3$`Pr(>|t|)`
      }
      lm3_data$lower <- lm3_data$estimate - z * lm3_data$std.error
      lm3_data$upper <- lm3_data$estimate + z * lm3_data$std.error
    } else{
      lm3_data <- as.data.frame(broom::tidy(lmz))
      lm3_data$lower <- lm3_data$estimate - z * lm3_data$std.error
      lm3_data$upper <- lm3_data$estimate + z * lm3_data$std.error
    }
    
    # Remove intercept xxx hier noch Option mit Intercept in Grafik
    lm3_data <- lm3_data[-1,]
    
    # Datensatz der Benennung der Variablen angibt
    # Hier gibt es jetzt die Funktion namenplot die das übernimmt
    
    unabz <- Reduce(paste, deparse(lmz$terms[[3]]))
    unabz <- gsub("[[:space:]]", "", unabz)
    unabz <- as.character(unlist(strsplit(unabz, "\\+")))
    
    
    beschriftung_neuz <- NULL
    for(i in 1:length(unabz)){
      beschriftungz <- namenplot(unabz[i], lm = lmz, englisch = englisch, file = file)
      beschriftung_neuz <- c(beschriftung_neuz, beschriftungz)
    }
    
    position1z <- length(beschriftung_neuz)+1
    
    # Plot
    
    # Fach zu Datensazu hinzufügen
    
    
    # GRAFIK ---------------------------------------------------------------------
    if(englisch){
      lm_data$fach <- "Subject 1"
      lm3_data$fach <- "Subject 2"
      
      lm_gesamt_st <- rbind(lm3_data, lm_data)
      
      lm_gesamt_st$fach <- factor(lm_gesamt_st$fach)
      
      p <- ggplot(data = lm_gesamt_st) + geom_point(aes(y=term,x = estimate, color =ifelse((p.value) < 0.05,"Yes","No")), size = 4) +
        geom_linerange(aes(xmin = lower, xmax = upper, y = term, color = ifelse((p.value) < 0.05,"Yes","No")), linetype = 1, size = 1) +
        geom_vline(xintercept = 0, linetype = 2) + facet_wrap(~fach) +
        scale_color_manual("Significant", breaks = c("No", "Yes"), values= sig_col) +
        scale_y_discrete(limits = rev(lm_data$term), labels=rev(beschriftung_neu)) +
        ylab("Explanatory variable") + xlab("Standardised beta coefficient")  +
        xlim(c(-0.6, 0.6)) +  theme_bw(base_size=16)
    }else{
      lm_data$fach <- "Fach 1"
      lm3_data$fach <- "Fach 2"
      
      lm_gesamt_st <- rbind(lm3_data, lm_data)
      lm_gesamt_st$fach <- factor(lm_gesamt_st$fach)
      
      p <- ggplot(data = lm_gesamt_st) + geom_point(aes(y=term,x = estimate, color =ifelse((p.value) < 0.05,"Ja","Nein")), size = 4) +
        geom_linerange(aes(xmin = lower, xmax = upper, y = term, color = ifelse((p.value) < 0.05,"Ja","Nein")), linetype = 1, size = 1) +
        geom_vline(xintercept = 0, linetype = 2) + facet_wrap(~fach) +
        scale_color_manual("Signifikant", values=rev(sig_col)) +
        scale_y_discrete(limits = rev(lm_data$term), labels=rev(beschriftung_neu)) +
        ylab("Unabhängige Variable") + xlab("Standardisierster Betakoeffizient")  +
        xlim(c(-0.6, 0.6)) +  theme_bw(base_size=16)
    }
    
    
    
    
  } # Ende 2. LM
  
  time <- gsub(":|-| ", "", Sys.time())
  if(speichern){
    ggsave(filename = paste0(pfad, "Grafik_",  time, ".png"), plot = p)
  }
  return(p)
  
}

