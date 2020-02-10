#===================================================================================#
# PARCC Plagiarism Detection Functions
options(scipen=999) # deactivate scientific notation

#===================================================================================#
# Internal function for calculating group-mean studentized residuals
groupMeans = function(group, outcome) {
  if(sum(as.numeric(!is.na(group)))==0) {
    return(data.frame("ID"=NA, "Name"=NA, "Group"=NA, "N"=NA, "Mean"=NA))
  } else {
    which.noID = which(substr(group,nchar(group),nchar(group))=="_")
    if(!identical(which.noID, integer(0))) group[which.noID] = paste(group[which.noID],"noID",sep="")
    
    means = tapply(outcome, group, mean)
    split.temp = strsplit(names(means), split="_")
    
    out.table = data.frame("ID"=rep(NA,length(means)))
    for(i in 1:length(means)) out.table$ID[i] = split.temp[[i]][length(split.temp[[i]])]
    out.table$Group = names(means)
    out.table$Group = as.character(out.table$Group)
    out.table$N = as.numeric(tapply(outcome, group, length))
    out.table$Mean = as.numeric(means)
    out.table$Group = gsub("noID","",out.table$Group)
    return(out.table)
  }
} #group means function end.

removeRows = function(dat, id) {
  for(j in 1:length(id)) dat = dat[which(dat$ID != id[j]),]
  return(dat)
} #remove function.



#===================================================================================#
findPDSample = function(testcode, tm.path, summ.path, conf.path=NULL, exp.states=NULL,
                        admin, N.compare=NULL, N.school.min=0) 
  {
  
    #-------------------------#
    # Read testmap
    dir.tm = dir(tm.path)
    dir.tm = dir.tm[grepl(testcode, toupper(dir.tm))]
    cat("\nTestmap:",dir.tm,"\n")
    filetype = strsplit(dir.tm, split="\\.")[[1]][[2]]
    if(filetype=="csv") tm = read.csv(paste(tm.path,"/",dir.tm,sep=""), stringsAsFactors=FALSE)
    if(filetype=="sas7bdat") tm = read.SAS(paste(tm.path,"/",dir.tm,sep=""))
    tm = tm[c("Form_ID","UIN","Section_Name","Objective_1","Max_Score_points","Item_Status")]
    tm = tm[tm$Item_Status=="OP",]
    tm = tm[which(tm$UIN != "" & !is.na(tm$Max_Score_points)),]
    tm = tm[which(substr(tm$Form_ID,9,9)!="P" & substr(tm$Form_ID,9,9)!="D"),] # keep only online forms
    tm = tm[which(tm$Max_Score_points > 2),] # keep only PCRs
    
    #-------------------------#
    # Read Summative Data Files
    dir.summ = dir(summ.path) # list of summ data files
    dir.summ = dir.summ[grepl(testcode, toupper(dir.summ))]
    cat("\nSummative Data File:",dir.summ,"\n")
    filetype = strsplit(dir.summ, split="\\.")[[1]][[2]]
    if(filetype=="csv") summ = read.csv(file=paste(summ.path,"/",dir.summ,sep=""), stringsAsFactors=FALSE)
    if(filetype=="sas7bdat") summ = read.SAS(filepath=paste(summ.path,"/",dir.summ,sep=""))
    summ = summ[summ$ONLINE_MODE!="",] # keep students who tested online
    
    # Remove unnecessary rows
    summ = summ[,!grepl("SubclaimCategory",names(summ))]
    summ = summ[,!grepl("StudentGrPerc",names(summ))]
    summ = summ[,!grepl("DateTime",names(summ))]
    
    # Add PCR UINs
    #tm$UIN = gsub("_TTS","",tm$UIN)
    #tm = tm[tm$Section_Name != "Unit 3a",]
    summ$UIN_LAT = NA; summ$UIN_RST = NA; summ$UIN_NWT = NA
    for(j in 1:nrow(tm)) {
        if(tm$Section_Name[j]=="Unit 1") summ$UIN_LAT[which(summ$FormID_UNIT1==tm$Form_ID[j])] = tm$UIN[j]
        if(tm$Section_Name[j]=="Unit 2") summ$UIN_RST[which(summ$FormID_UNIT2==tm$Form_ID[j])] = tm$UIN[j]
        if(tm$Section_Name[j]=="Unit 3" | tm$Section_Name[j]=="Unit 3a")
          summ$UIN_NWT[which(summ$FormID_UNIT3==tm$Form_ID[j])] = tm$UIN[j]
    } # for loop
  
    #-------------------------#
    # Read list of schools for confirmatory analysis
    if(!is.null(conf.path)) {
        dir.conf = dir(conf.path)
        cat("\nConfirmatory Analysis Lists:\n",dir.conf,"\n")
        library(xlsx)
        conf.cols = c("Admin","Period","Year","State","Testing_District_Code","District_Name","Testing_School_Code","School_Name")
        conf = read.xlsx(paste(conf.path,"/",dir.conf[1],sep=""), sheetIndex=1)
        names(conf)[1:length(conf.cols)] = conf.cols
        conf = conf[conf.cols]
        for(j in 1:ncol(conf)) conf[,j] = as.character(conf[,j])
          if(length(dir.conf) > 1) for(i in 2:length(dir.conf)) {
            conf.temp = read.xlsx(paste(conf.path,"/",dir.conf[i],sep=""), sheetIndex=1)
            names(conf.temp)[1:length(conf.cols)] = conf.cols
            conf.temp = conf.temp[conf.cols]
            for(j in 1:ncol(conf)) conf.temp[,j] = as.character(conf.temp[,j])
            conf = rbind(conf, conf.temp)  
            rm(conf.temp)
         } # for loop
    } #if else.
    
    #-------------------------#
    # Fit OLS regression Models
    summ = summ[!is.na(summ$TestWritingScaleScore) & !is.na(summ$TestReadingScaleScore),]
    summ$State[summ$State=="BI"] = 1
    summ$State[summ$State=="CO"] = 2
    summ$State[summ$State=="DC"] = 3
    summ$State[summ$State=="IL"] = 4
    summ$State[summ$State=="MA"] = 5
    summ$State[summ$State=="MD"] = 6
    summ$State[summ$State=="NJ"] = 7
    summ$State[summ$State=="NM"] = 8
    summ$State[summ$State=="RI"] = 9
    summ$State = factor(summ$State, levels=c(1:9), labels=c("BI","CO","DC","IL","MA","MD","NJ","NM","RI"))
    N.table = table(summ$State)
  
    cat("\nNumber of students in each state in summative data file:")
    print(N.table)
    
    summ$res = NA
    summ$studres = NA
    
    require(MASS)
    model1 = list()
    for(i in 1:length(N.table)) {
      if(N.table[i] > 1) {
        which.temp = which(summ$State==names(N.table)[i])
        model1[[i]] = lm(TestWritingScaleScore ~ TestReadingScaleScore, data=summ[which.temp,])
        summ$res[which.temp] = model1[[i]]$residuals
        summ$studres[which.temp] = studres(model1[[i]])
      }
    } #for loop.
    
    #-------------------------#
    
    
    cat("\nCheck for unusual school or district values (add to remove list)...\n")
    staff.table = list()
    school.table = list()
    district.table = list()
    
    summ$UniqueTestAdministrator = NA
    summ$UniqueSchoolCode = NA
    summ$UniqueDistrictCode = NA
    
    for(i in 1:length(model1)) {
      if(N.table[i] > 1) {
        which.temp = which(summ$State==names(N.table)[i])
        summ$UniqueTestAdministrator[which.temp] = paste(summ$State[which.temp],
                                                         summ$TestingDistrictCode[which.temp],
                                                         summ$TestingSchoolCode[which.temp],
                                                         summ$TestAdministrator[which.temp], sep="_")
        summ$UniqueSchoolCode[which.temp] = paste(summ$State[which.temp],
                                                         summ$TestingDistrictCode[which.temp],
                                                         summ$TestingSchoolCode[which.temp], sep="_")
        summ$UniqueDistrictCode[which.temp] = paste(summ$State[which.temp],
                                                  summ$TestingDistrictCode[which.temp], sep="_")
        
        staff.table[[i]] = groupMeans(group=summ$UniqueTestAdministrator[which.temp], outcome=summ$studres[which.temp])
        school.table[[i]] = groupMeans(group=summ$UniqueSchoolCode[which.temp], outcome=summ$studres[which.temp])
        district.table[[i]] = groupMeans(group=summ$UniqueDistrictCode[which.temp], outcome=summ$studres[which.temp])
        
        #cat("\nStaff in",names(N.table)[i],"\n")
        #print(names(table(staff.table[[i]]$ID)))
        #cat("\nSchools in",names(N.table)[i],"\n")
        #print(names(table(school.table[[i]]$ID)))
        #cat("\nDistricts in",names(N.table)[i],"\n")
        #print(names(table(district.table[[i]]$ID)))
        
        if(!is.na(staff.table[[i]]$ID[1])) staff.table[[i]] = removeRows(dat=staff.table[[i]], id=c("","0","000000","TBD","NA","noID"))
        school.table[[i]] = removeRows(dat=school.table[[i]], id=c("NO MATCH","BBBB","9999","NO MATCH"))
        district.table[[i]] = removeRows(dat=district.table[[i]], id=c("NO MATCH","NO MATCH 04","FFFF","YYYY"))
      }  
    } # for loop.
    
    # Add group-level residuals back to summ
    summ$N.admin = NA; summ$Mean.admin = NA
    summ$N.school = NA; summ$Mean.school = NA
    summ$N.district = NA; summ$Mean.district = NA
  
    cat("\nAdding group means back to summative data set...\n")
    for(i in 1:length(N.table)) {
      if(N.table[i] > 0) {
        flush.console()
        cat(names(N.table)[i],"\n")
        if(nrow(staff.table[[i]]) > 0) {
          for(j in 1:nrow(staff.table[[i]]))
            summ[which(summ$UniqueTestAdministrator==staff.table[[i]]$Group[j]), c("N.admin","Mean.admin")] =
              staff.table[[i]][j,3:4]
        }  
  
        if(nrow(school.table[[i]]) > 0) {
          for(j in 1:nrow(school.table[[i]]))
            summ[which(summ$UniqueSchoolCode==school.table[[i]]$Group[j]), c("N.school","Mean.school")] =
              school.table[[i]][j,3:4]
        } 
        
        if(nrow(district.table[[i]]) > 0) {
          for(j in 1:nrow(district.table[[i]]))
            summ[which(summ$UniqueDistrictCode==district.table[[i]]$Group[j]), c("N.district","Mean.district")] =
              district.table[[i]][j,3:4]
        } 
        
      }
    } # for loop.
    
    if(nrow(summ) > 0) summ = summ[!is.na(summ$Mean.school),] # Remove data with no Mean.school value
  
    #-------------------------#
    #save.image("Temp.RData")
    #load("Temp.RData")
    #-------------------------#
    
    # Determine what proportion of results to take from each state
    filelist = data.frame("State"=names(N.table))
    filelist$N = as.numeric(N.table)
  
    filelist$exp = FALSE
    if(!is.null(exp.states)) {
      for(i in 1:length(exp.states)) filelist$exp[filelist$State==exp.states[i]] = TRUE
      filelist$N[!filelist$exp] = 0
    }
    
    filelist$Weight = filelist$N/sum(filelist$N)
    if(is.null(N.compare)) {
      filelist$N_max = filelist$N
    } else {
      filelist$N_max = round(N.compare*filelist$Weight)
    }
    filelist$N_max[filelist$N_max > filelist$N] = filelist$N[filelist$N_max > filelist$N]
    
    cat("\nN.compare =",N.compare,"\n")
    print(filelist)
    
    # Remove schools with fewer than N.school.min
    if(N.school.min > 0) {
      summ$Analyze = "Y"
      school.table = table(summ$UniqueSchoolCode)
      N.table = data.frame("UniqueSchoolCode"=names(school.table), "N"=as.numeric(school.table))
      N.table$Analyze = ifelse(N.table$N >= N.school.min,"Y","N")
      N.table = N.table[which(N.table$Analyze=="N"),]
      N.table$UniqueSchoolCode = as.character(N.table$UniqueSchoolCode)
      if(nrow(N.table) > 0) for(j in 1:nrow(N.table))
        summ$Analyze[which(summ$UniqueSchoolCode==N.table$UniqueSchoolCode[j])] = "N"
      summ = summ[-which(summ$Analyze=="N"),]
      summ = summ[,-which(names(summ)=="Analyze")]
    }
  
    # Identify schools for analysis
    cat("\nIdentify schools within states for analysis:\n")
    
    summ.all = summ
    summ.all$PoolId = NA
    summ.all$Compare = "N"
    summ = list() # split summ into separate data sets by state
    for(i in 1:nrow(filelist)) summ[[i]] = summ.all[summ.all$State==filelist$State[i],]
  
    pool.count = 1
    for(i in 1:length(summ)) {
      if(filelist$N[i] >= 2 & filelist$N_max[i] > 0) {
        flush.console()      
        N.temp = 0
        N_max = filelist$N_max[i]
        flag = TRUE      
        while(flag & nrow(summ[[i]][which(summ[[i]]$Compare=="N"),]) > 0) { # while there are still schools remaining to select
          max.temp = max(summ[[i]]$Mean.school[which(summ[[i]]$Compare=="N")], na.rm=TRUE) # temp max mean residual
          
          which.temp = which(summ[[i]]$Mean.school==max.temp & summ[[i]]$Compare=="N") # indices of students in school with max mean residual
          # possibly picking multiple schools
          
          if(N.temp==0 | ((N.temp + length(which.temp)) <= N_max)) {
            # Check for multiple schools with same mean
            school.temp = names(table(summ[[i]]$UniqueSchoolCode[which.temp]))
            if(length(school.temp) > 1) {
              print("WARNING: Multiple schools with the same maximum average residual found!")
              print(school.temp, quote=FALSE)
              print(table(summ[[i]]$Mean.school[which.temp]))
            }
            
            for(j in 1:length(school.temp)) {
              which.school.temp = which(summ[[i]]$UniqueSchoolCode==school.temp[j])
              # Verify that multiple students responded to some prompt within the selected school
              if(max(table(summ[[i]]$UIN_LAT[which.school.temp])) >= 2 | 
                 max(table(summ[[i]]$UIN_RST[which.school.temp])) >= 2 |
                 max(table(summ[[i]]$UIN_NWT[which.school.temp])) >= 2) {
                N.temp = N.temp + length(which.school.temp) # cumulative number of students selected for comparison
                summ[[i]]$Compare[which.school.temp] = "Y"
                summ[[i]]$PoolId[which.school.temp] = pool.count
                pool.count = pool.count + 1
              } else {
                summ[[i]] = summ[[i]][-which.school.temp,] # remove that school
              }
            }
    
          } else {
            flag = FALSE
          }
        }
        cat(paste(filelist$State[i]," had ",sum(as.numeric(summ[[i]]$Compare=="Y")),
                    " responses selected out of a maximum of ", N_max, " for the exploratory analysis.\n", sep=""))
      }
    } #end of outer for loop.
  
    #-------------------------#
    # Add schools for confirmatory analysis
    for(i in 1:nrow(filelist)) {
      conf.temp = conf[as.character(conf$State)==filelist$State[i],]
      
      # Remove duplicate rows
      conf.temp$UniqueID = paste(conf.temp$Testing_District_Code, conf.temp$Testing_School_Code, sep="_")
      conf.temp = conf.temp[!duplicated(conf.temp$UniqueID),]
      
      if(nrow(conf.temp) > 0) {
        
        for(j in 1:nrow(conf.temp)) {
          which.temp = which(summ[[i]]$TestingDistrictCode==conf.temp$Testing_District_Code[j] &
                               summ[[i]]$TestingSchoolCode==conf.temp$Testing_School_Code[j])
          if(!identical(which.temp, integer(0))) {
            cat("\nSchool found. District =",conf.temp$Testing_District_Code[j],
                "- School =",conf.temp$Testing_School_Code[j],conf.temp$School_Name[j])
            summ[[i]]$Compare[which.temp] = "Y"
            summ[[i]]$PoolId[which.temp] = pool.count
            pool.count = pool.count + 1    
          } else {
            cat("\n  SCHOOL NOT FOUND! District =",conf.temp$Testing_District_Code[j],
                "- School =",conf.temp$Testing_School_Code[j],conf.temp$School_Name[j])
          }
        }
        
        cat("\n",paste(filelist$State[i]," had ",sum(as.numeric(summ[[i]]$Compare=="Y")),
                    " responses selected for the confirmatory analysis.\n", sep=""))
        cat("\n",paste(filelist$State[i]," had ",nrow(summ[[i]]),
                       " students in the summative data file.\n", sep=""))
        cat("TestingSchoolCode:\n")
        print(table(summ[[i]]$TestingSchoolCode))
        cat("ResponsibleSchoolCode:\n")
        print(table(summ[[i]]$ResponsibleSchoolCode))
      }
    }
    
    # combine summs
    summ.all = summ[[1]]
    if(length(summ) > 1) for(i in 2:length(summ)) summ.all = rbind(summ.all,summ[[i]])
    summ.all = summ.all[which(summ.all$Compare=="Y"),]
    summ.all = summ.all[,-which(names(summ.all)=="Compare")] 
    summ.all = summ.all[order(summ.all$State, summ.all$PoolId),]
  
    #-------------------------#
    # Generate Output
    
    # Internal function for formatting a summative data file before output
    formatsumm = function(summ, task.type) {
      if(task.type=="LAT") { StudentUnitTestUUID = "StudentUnitTestUUID1"; FormID = "FormID_UNIT1" }
      if(task.type=="RST") { StudentUnitTestUUID = "StudentUnitTestUUID2"; FormID = "FormID_UNIT2" }
      if(task.type=="NWT") { StudentUnitTestUUID = "StudentUnitTestUUID3"; FormID = "FormID_UNIT3" }
      
      summ = summ[c("State","ResponsibleDistrictCode","ResponsibleSchoolCode",
                    "TestingDistrictCode","TestingSchoolCode","TestAdministrator",
                    "StateStudentIdentifier","PARCCStudentIdentifier",
                    "StudentTestUUID",StudentUnitTestUUID, FormID,
                    "TestWritingScaleScore","TestReadingScaleScore",
                    "res","studres","N.admin","Mean.admin","N.school","Mean.school","N.district","Mean.district","PoolId")]
      names(summ)[grepl("StudentUnitTestUUID",names(summ))] = "StudentUnitTestUUID"
      names(summ)[grepl("FormID_UNIT",names(summ))] = "FormID_UNIT"
      
      return(summ)
    }
    
    filename = paste(admin, testcode, sep="_")
    
    # Save LAT files
    summ.LAT = list()
    UIN_LAT = names(table(summ.all$UIN_LAT))
    print("LAT Table:", quote=FALSE)
    print(table(summ.all$UIN_LAT))
    for(j in 1:length(UIN_LAT)) {
      flush.console()
      print(paste("LAT:",UIN_LAT[j]), quote=FALSE)
      summ.LAT[[j]] = formatsumm(summ=summ.all[which(summ.all$UIN_LAT==UIN_LAT[j]),], task.type="LAT")
      summ.LAT[[j]]$PCR_Type = "LAT"
      summ.LAT[[j]]$UIN = UIN_LAT[j]
      summ.LAT[[j]]$PoolId = formatC(summ.LAT[[j]]$PoolId, width=3, flag="0")
      
      # Remove data for schools with fewer than 2 students responding to this PCR
      summ.LAT[[j]]$UniqueId = paste(summ.LAT[[j]]$State, summ.LAT[[j]]$TestingDistrictCode, summ.LAT[[j]]$TestingSchoolCode, sep="_")
      school.table = table(summ.LAT[[j]]$UniqueId)
      which.drop = which(as.numeric(school.table) < 2)
      if(!identical(which.drop,integer(0))) {
        school.drop = names(school.table)[which.drop]
        for(k in 1:length(school.drop)) summ.LAT[[j]] = summ.LAT[[j]][-which(summ.LAT[[j]]$UniqueId==school.drop[k]),]
      }
      summ.LAT[[j]] = summ.LAT[[j]][,-which(names(summ.LAT[[j]])=="UniqueId")]
      
      write.csv(summ.LAT[[j]], file=paste("./Output/",filename,"_LAT_",UIN_LAT[j],".csv",sep=""), row.names=FALSE, na="")
    }
    
    # Save RST files
    summ.RST = list()
    UIN_RST = names(table(summ.all$UIN_RST))
    print("RST Table:", quote=FALSE)
    print(table(summ.all$UIN_RST))
    for(j in 1:length(UIN_RST)) {
      flush.console()
      print(paste("RST:",UIN_RST[j]), quote=FALSE)
      summ.RST[[j]] = formatsumm(summ=summ.all[which(summ.all$UIN_RST==UIN_RST[j]),], task.type="RST")
      summ.RST[[j]]$PCR_Type = "RST"
      summ.RST[[j]]$UIN = UIN_RST[j]
      summ.RST[[j]]$PoolId = formatC(summ.RST[[j]]$PoolId, width=3, flag="0")
      
      # Remove data for schools with fewer than 2 students responding to this PCR
      summ.RST[[j]]$UniqueId = paste(summ.RST[[j]]$State, summ.RST[[j]]$TestingDistrictCode, summ.RST[[j]]$TestingSchoolCode, sep="_")
      school.table = table(summ.RST[[j]]$UniqueId)
      which.drop = which(as.numeric(school.table) < 2)
      if(!identical(which.drop,integer(0))) {
        school.drop = names(school.table)[which.drop]
        for(k in 1:length(school.drop)) summ.RST[[j]] = summ.RST[[j]][-which(summ.RST[[j]]$UniqueId==school.drop[k]),]
      }
      summ.RST[[j]] = summ.RST[[j]][,-which(names(summ.RST[[j]])=="UniqueId")]
      
      write.csv(summ.RST[[j]], file=paste("./Output/",filename,"_RST_",UIN_RST[j],".csv",sep=""), row.names=FALSE, na="")
    }
    
    # Save NWT files
    summ.NWT = list()
    UIN_NWT = names(table(summ.all$UIN_NWT))
    print("NWT Table:", quote=FALSE)
    print(table(summ.all$UIN_NWT))
    for(j in 1:length(UIN_NWT)) {
      flush.console()
      print(paste("NWT:",UIN_NWT[j]), quote=FALSE)
      summ.NWT[[j]] = formatsumm(summ=summ.all[which(summ.all$UIN_NWT==UIN_NWT[j]),], task.type="NWT")
      summ.NWT[[j]]$PCR_Type = "NWT"
      summ.NWT[[j]]$UIN = UIN_NWT[j]
      summ.NWT[[j]]$PoolId = formatC(summ.NWT[[j]]$PoolId, width=3, flag="0")
      
      # Remove data for schools with fewer than 2 students responding to this PCR
      summ.NWT[[j]]$UniqueId = paste(summ.NWT[[j]]$State, summ.NWT[[j]]$TestingDistrictCode, summ.NWT[[j]]$TestingSchoolCode, sep="_")
      school.table = table(summ.NWT[[j]]$UniqueId)
      which.drop = which(as.numeric(school.table) < 2)
      if(!identical(which.drop,integer(0))) {
        school.drop = names(school.table)[which.drop]
        for(k in 1:length(school.drop)) summ.NWT[[j]] = summ.NWT[[j]][-which(summ.NWT[[j]]$UniqueId==school.drop[k]),]
      }
      summ.NWT[[j]] = summ.NWT[[j]][,-which(names(summ.NWT[[j]])=="UniqueId")]
      
      write.csv(summ.NWT[[j]], file=paste("./Output/",filename,"_NWT_",UIN_NWT[j],".csv",sep=""), row.names=FALSE, na="")
    }

} #end of function.


#===================================================================================#
aggregateCourse = function(kt.path, course) {
  dir.kt = dir(kt.path)
  dir.kt = dir.kt[grepl(course, dir.kt)]
  
  # Input and combine files from KT
  kt = list()
  for(i in 1:length(dir.kt)) {
    kt[[i]] = read.csv(paste(kt.path, "/", dir.kt[i], sep=""), stringsAsFactors=FALSE)
  }
  kt.all = kt[[1]]
  if(length(kt) > 1) for(i in 2:length(kt)) kt.all = rbind(kt.all, kt[[i]])
  
  kt.all$UniqueSchoolId = paste(kt.all$State, kt.all$TestingDistrictCode, kt.all$TestingSchoolCode, sep="_")
  kt.all = kt.all[order(kt.all$State, kt.all$PoolId),]
  kt.all$MatchList[which(kt.all$MatchList=="")] = NA # set blank MatchList to NA
  
  kt.all$MatchCount = suppressWarnings(as.numeric(kt.all$MatchCount)) # set non-numeric (e.g., "M") MatchCount to NA
  kt.all = kt.all[!is.na(kt.all$MatchCount),] # Remove students with blank responses
  
  print("Table of Matches:", quote=FALSE)
  print(table(paste(kt.all$PCR_Type[which(kt.all$MatchCount > 0)], kt.all$UIN[which(kt.all$MatchCount > 0)], sep="_")))
  
  out = data.frame("UniqueSchoolId"=names(table(kt.all$UniqueSchoolId)))
  out$Course = course
  out$StateAbbreviation = NA
  out$TestingSchoolCode = NA
  out$ResponsibleSchoolCode = NA
  out$TestingDistrictCode = NA
  out$ResponsibleDistrictCode = NA
  out$SchoolN = NA
  out$SchoolAvgStudRes = NA
  out$MatchCount = NA
  out$MatchCountLAT = NA; out$MatchCountRST = NA; out$MatchCountNWT = NA
  out$PctMatch = NA
  out$MatchListLAT = NA; out$MatchListRST = NA; out$MatchListNWT = NA
  
  for(i in 1:nrow(out)) {
    kt.temp = kt.all[which(kt.all$UniqueSchoolId==out$UniqueSchoolId[i]),]
    kt.temp$MatchCount = suppressWarnings(as.numeric(kt.temp$MatchCount))
    
    # Count possible number of matches
    N.obs = 0
    N.possible = 0
    UIN.table = table(kt.temp$UIN)
    UIN = names(UIN.table)
    for(j in 1:length(UIN)) {
      if(UIN.table[j] > 1) N.possible = N.possible + choose(n=UIN.table[j], k=2)
    }
    
    out$StateAbbreviation[i] = kt.temp$State[1]
    out$TestingSchoolCode[i] = kt.temp$TestingSchoolCode[1]
    out$ResponsibleSchoolCode[i] = kt.temp$ResponsibleSchoolCode[1]
    out$TestingDistrictCode[i] = kt.temp$TestingDistrictCode[1]
    out$ResponsibleDistrictCode[i] = kt.temp$ResponsibleDistrictCode[1]
    out$SchoolN[i] = kt.temp$N.school[1]
    out$SchoolAvgStudRes[i] = kt.temp$Mean.school[1]
    
    out$MatchCount[i] = sum(kt.temp$MatchCount, na.rm=TRUE)  
    out$MatchCountLAT[i] = sum(kt.temp$MatchCount[which(kt.temp$PCR_Type=="LAT")], na.rm=TRUE)  
    out$MatchCountRST[i] = sum(kt.temp$MatchCount[which(kt.temp$PCR_Type=="RST")], na.rm=TRUE)  
    out$MatchCountNWT[i] = sum(kt.temp$MatchCount[which(kt.temp$PCR_Type=="NWT")], na.rm=TRUE)  
    
    if(out$MatchCount[i] > 0) {
      kt.temp = kt.temp[which(kt.temp$MatchCount > 0),]
      print(paste("i=", i, ": ", nrow(kt.temp), " row(s) with matches.", sep=""), quote=FALSE)
      
      MatchListLAT.temp = ""
      MatchListRST.temp = ""
      MatchListNWT.temp = ""
      
      for(j in 1:nrow(kt.temp)) {
        type.temp = kt.temp$PCR_Type[j]
        matchlist.temp = strsplit(kt.temp$MatchList[j], split=",")[[1]]
        N.obs = N.obs + length(matchlist.temp)
        if(type.temp=="LAT") MatchListLAT.temp = c(MatchListLAT.temp,
                                                   paste(type.temp, "_", kt.temp$UIN[j], "_",
                                                         kt.temp$StudentUnitTestUUID[j], "_", 
                                                         matchlist.temp, sep=""))
        if(type.temp=="RST") MatchListRST.temp = c(MatchListRST.temp,
                                                   paste(type.temp, "_", kt.temp$UIN[j], "_",
                                                         kt.temp$StudentUnitTestUUID[j], "_", 
                                                         matchlist.temp, sep=""))
        if(type.temp=="NWT") MatchListNWT.temp = c(MatchListNWT.temp,
                                                   paste(type.temp, "_", kt.temp$UIN[j], "_",
                                                         kt.temp$StudentUnitTestUUID[j], "_", 
                                                         matchlist.temp, sep=""))
      }
      if(type.temp=="LAT") out$MatchListLAT[i] = paste(MatchListLAT.temp[-1], collapse="; ") 
      if(type.temp=="RST") out$MatchListRST[i] = paste(MatchListRST.temp[-1], collapse="; ") 
      if(type.temp=="NWT") out$MatchListNWT[i] = paste(MatchListNWT.temp[-1], collapse="; ") 
    }
    out$PctMatch[i] = round(N.obs/N.possible*100,2)
  }
  
  out = out[order(out$StateAbbreviation, out$UniqueSchoolId),]
  out = out[order(out$MatchCount,decreasing=TRUE),]
  out = out[,-which(names(out)=="UniqueSchoolId")]
  
  return(out)
}

#===================================================================================#
cleansumm = function(summ) {
  summ = summ[,c(10:25,49,97:105,136:144)] # keep only columns needed
  names(summ) = gsub("pba","PBA",names(summ)) # upper-case PBA
  names(summ) = gsub("eoy","EOY",names(summ)) # upper-case EOY
  substr(names(summ),1,1) = toupper(substr(names(summ),1,1)) # Change first character to upper-case
  
  pba.form.type = substr(summ$PBAFormId,10,10)
  eoy.form.type = substr(summ$EOYFormId,10,10)
  #print(table(pba.form.type,eoy.form.type))
  # Include only students who took PBA and EOY in the same condition
  which.OT = which((pba.form.type=="O" & eoy.form.type=="O") | (pba.form.type=="T" & eoy.form.type=="T"))
  summ = summ[which.OT,]
  
  return(summ)
}

#===================================================================================#


#===================================================================================#


#===================================================================================#
read.SAS = function(filepath, sas.path=NULL, temp.path=NULL, keep.csv=TRUE) {
  if(is.null(sas.path)) sas.path = "C:/Program Files/SAS/SASFoundation/9.2(32-bit)"
  if(is.null(temp.path)) temp.path = "C:/"
  if(substr(temp.path,nchar(temp.path),nchar(temp.path)) != "/") temp.path = paste(temp.path,"/",sep="")
  current.path = getwd()
  
  # if . or .. shorthand is used, replace with full path (SAS cannot accept . or ..)
  if(substr(filepath,1,3)=="../") {
    setwd("../")
    path.temp = getwd()
    filepath = paste(path.temp,gsub("\\.\\.","",filepath),sep="")
    setwd(current.path)
  }
  if(substr(filepath,1,2)=="./") {
    filepath = paste(current.path,gsub("\\./","",paste("/",filepath,sep="")),sep="")
  }
  
  split.temp = strsplit(filepath, split="/")[[1]]
  if(split.temp[1]==".") split.temp[1] = current.path
  filename = split.temp[length(split.temp)]
  filename = strsplit(filename, split="\\.")[[1]][1] # remove sas7bdat extension
  split.temp = split.temp[-length(split.temp)] # remove filename
  fullpath = paste(split.temp, collapse="/")
  
  sas = c()
  sas[1] = "options compress=no;"
  fullpath = gsub("/","\\\\",fullpath)
  sas[2] = paste("LIBNAME TempLib \'",fullpath,"\';",sep="")
  sas[3] = paste("LIBNAME CLib '",gsub("/","\\\\",temp.path),"';",sep="")
  sas[4] = "data CLib.uncompressed_data;"
  sas[5] = paste("set TempLib.",filename,";",sep="")
  sas[6] = "run;"
  sas[7] = "proc export data=CLib.uncompressed_data"
  sas[8] = paste("outfile=\'",gsub("/","\\\\",temp.path),"temp_uncompressed.csv\'",sep="")
  sas[9] = "dbms=csv"
  sas[10] = "replace;"
  sas[11] = "run;"
  
  write(sas, file=paste(temp.path,"SAStoCSV_temp.sas",sep=""))
  setwd(sas.path)
  command = paste("sas -BATCH -LOG ",temp.path,"temp_log.log -SYSIN ",
                  temp.path,"SAStoCSV_temp.sas", sep="")
  system(command)
  setwd(current.path)
  
  out = read.csv(paste(temp.path,"temp_uncompressed.csv",sep=""), stringsAsFactors=FALSE)
  if(keep.csv) {
    file.rename(from=paste(temp.path,"temp_uncompressed.csv",sep=""),
                to=paste(temp.path,filename,".csv",sep=""))
  } else {
    file.remove(paste(temp.path,"temp_uncompressed.csv",sep=""))
  }
  file.remove(paste(temp.path,"uncompressed_data.sas7bdat",sep=""))
  file.remove(paste(temp.path,"SAStoCSV_temp.sas",sep=""))
  file.remove(paste(temp.path,"temp_log.log",sep=""))
  
  return(out)
}

#===================================================================================#
