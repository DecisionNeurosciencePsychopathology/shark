load("shark_rawdata.rdata")
source("~/Documents/UPMC/RStation/Jiazhou.Startup.R")
startup()
masterdemo<-bsrc.checkdatabase2(ptcs$masterdemo,online = T,forceupdate = T,output = T,batch_size = 1000)
pt_idmap <- bsrc.getform(formname = "record_registration",curdb = masterdemo)[c("registration_redcapid","registration_wpicid")]
names(pt_idmap)<-c("redcap_id","wpic_id")

ts_idmap <- bsrc.findid(data.frame(ID = names(shark_data),stringsAsFactors = F),idmap = pt_idmap,id.var = "ID")
if(any(!ts_idmap$ifexist)){stop(paste(ts_idmap[!ts_idmap$ifexist,c("ID")],collapse = ", ")," do not have redcap records")}

explore_demo <- bsrc.getform(formname = "record_registration",curdb=masterdemo)
explore_demo <- explore_demo[which(explore_demo$registration_redcapid %in% ts_idmap$redcap_id),]
protect <- bsrc.checkdatabase2(protocol = ptcs$protect,forceupdate = T)

subject_df<-data.frame(redcapid=explore_demo$registration_redcapid,
                       id=ts_idmap$ID[match(ts_idmap$redcap_id,explore_demo$registration_redcapid)],
                       age=round(as.numeric((as.Date(explore_demo$reg_condate_explore) - as.Date(explore_demo$registration_dob))/365.25),2),
                       female=explore_demo$registration_gender=="F",
                       edu = as.numeric(explore_demo$registration_edu),
                       scandate=as.Date(explore_demo$reg_condate_explore),GroupATT=explore_demo$registration_group,
                       Lethality=explore_demo$registration_lethality)
subject_df$Group<-factor(subject_df$GroupATT,levels = c("HC","DEP","IDE","ATT","DNA"))
subject_df <- subject_df[which(subject_df$id %in% names(shark_data)),]
# lapply(c("neoffi","bis36"),function(x){
#   dfx <- bsrc.getform(curdb = protect,formname = x)
#   dfy<-bsrc.score_this_form(df_in = dfx,formname = x)
#   names(dfy)[1:2] <- c("ID","EVT"); dfy$EVT<-NULL
#
# })
protect_neoraw <- bsrc.getform(formname = "neoffi",curdb=protect)
protect_neoraw<-do.call(rbind,lapply(split(protect_neoraw,protect_neoraw$registration_redcapid),function(x){
  x[1,]
}))

protect_neoraw <- bsrc.score_this_form(protect_neoraw,"neoffi")
protect_neoraw$redcap_event_name <- NULL
subject_df <- merge(subject_df,protect_neoraw,by.x = "redcapid",by.y = "registration_redcapid",all.x = T)



protect_neuro_psy<-lapply(c("wtar","exit","drs","mmse"),bsrc.getform,protocol=NULL,curdb=protect,no_calc = F)
names(protect_neuro_psy) <- c("wtar","exit","drs","mmse")
protect_neuro_psy$exit$exit_total[is.na(protect_neuro_psy$exit$exit_total)] <- protect_neuro_psy$exit$exit_raw[is.na(protect_neuro_psy$exit$exit_total)]

IDDateMap <- bsrc.getIDDateMap(db = protect,return_id_sp = TRUE)
protect_combo_psy<-do.call(rbind,lapply(subject_df$redcapid,function(ID){
  s_date <- subject_df$scandate[subject_df$redcapid==ID]
  rc_dates <- IDDateMap[[as.character(ID)]]
  f_al<-do.call(cbind,lapply(list(exit = rc_na_remove(protect_neuro_psy$exit[protect_neuro_psy$exit$registration_redcapid==ID,c("exit_total","redcap_event_name")]),
                                  wtar = rc_na_remove(protect_neuro_psy$wtar[protect_neuro_psy$wtar$registration_redcapid==ID,c("wtar_s","redcap_event_name")]),
                                  drs = rc_na_remove(protect_neuro_psy$drs[protect_neuro_psy$drs$registration_redcapid==ID,c("drs_total","redcap_event_name")]),
                                  mmse = rc_na_remove(protect_neuro_psy$mmse[protect_neuro_psy$mmse$registration_redcapid==ID,c("mmse_s_adj","redcap_event_name")])
  ),function(xa){
    if(nrow(xa)>1){
      if (any(lgx<-grepl("explore",xa$redcap_event_name))) {
        xb <- xa[lgx,]
      } else {
        xb <-xa[which.min(abs(as.Date(rc_dates$date[match(xa$redcap_event_name,rc_dates$redcap_event_name)]) - s_date)),]
      }
    } else if (nrow(xa) < 1) {
      xb <- xa
      xb[1,]<-NA
    } else {
      xb <- xa
    }
    
    xb$redcap_event_name <- NULL
    return(xb)
  }))
  f_al$ID <- ID
  return(f_al)
}))

subject_df <- merge(subject_df,protect_combo_psy,by.x = "redcapid",by.y = "ID",all.x = T)

protect_ssi <-bsrc.matchIDDate(dfx =  bsrc.getform(formname = "ssi_scale_of_suicidal_ideation",
                                                   curdb = protect),db = protect)
protect_ssi <- bsrc.score_this_form(df_in = protect_ssi,formname = "ssi_scale_of_suicidal_ideation",aggregate_by_subj=TRUE)
protect_ssi <- protect_ssi[c("registration_redcapid","ssi_score_worst_lifetime")]
subject_df <- merge(subject_df,protect_ssi,by.x = "redcapid",by.y = "registration_redcapid",all.x = T)

ssi_medi<-median(subject_df$ssi_score_worst_lifetime[subject_df$Group %in% c("IDE","DEP","DNA")])

subject_df$GroupNEW <- as.character(subject_df$Group)
subject_df$GroupNEW[subject_df$Group %in% c("IDE","DEP","DNA") & subject_df$ssi_score_worst_lifetime >= ssi_medi] <- "DNA_HI"
subject_df$GroupNEW[subject_df$Group %in% c("IDE","DEP","DNA") & subject_df$ssi_score_worst_lifetime < ssi_medi] <- "DNA_LI"

save(subject_df,file = file.path("~/Box/skinner/data/MRI/shark","subj_df.rdata"))
save(subject_df,file = file.path("explore_subj_df.rdata"))
