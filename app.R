#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

convert.to.numeric.semester <- function(semester) {
  year <- strsplit(semester, " ")[[1]][2] 
  suffix <- ".00"
  if(grepl("Fall", semester)) {
    suffix <- ".08" 
  }
  if(grepl("Spring", semester)) {
    suffix <- ".01" 
  }
  if(grepl("Summer", semester)) {
    suffix <- ".05" 
  }
  return(paste0(year,suffix))
}

convert.to.text.semester <- function(semester) {
  year <- strsplit(semester, '\\.')[[1]][1] 
  prefix <- "None "
  if(grepl(".08", semester)) {
    prefix <- "Fall" 
  }
  if(grepl(".01", semester)) {
    prefix <- "Spring" 
  }
  if(grepl(".05", semester)) {
    prefix <- "Summer" 
  }
  return(paste0(prefix, " ", year))
}

course.semester.pairs.generate <- function(file="Courses.txt") {
  course.grid <- as.data.frame(readit::readit(file),stringsAsFactors=FALSE)
  colnames(course.grid) <- sapply(colnames(course.grid), convert.to.numeric.semester)
  course.semester.pairs <- data.frame(matrix(nrow=0, ncol=2))
  for (semester.index in sequence(ncol(course.grid))) {
    for (course.index in sequence(nrow(course.grid))) {
      if(!is.na(course.grid[course.index, semester.index])) {
        course.semester.pairs <- rbind(course.semester.pairs, c(course.grid[course.index,semester.index], colnames(course.grid)[semester.index]), stringsAsFactors=FALSE)
      }
    }
  }
  colnames(course.semester.pairs) <- c("Course", "Semester")
  course.semester.pairs <- course.semester.pairs[order(course.semester.pairs$Semester),]
  course.semester.pairs$Semester <- as.character(course.semester.pairs$Semester)
  return(course.semester.pairs)
}

read.prereqs <- function(file="Prereq.txt") {
  prereqs <- as.data.frame(readit::readit(file),stringsAsFactors=FALSE)
  return(prereqs)
}

read.coreqs <- function(file="Coreq.txt") {
  coreqs <- as.data.frame(readit::readit(file),stringsAsFactors=FALSE)
  return(coreqs)
}


#course.preferences: weight of taking that course (from 0 to 1)
generate.schedule <- function(course.semester.pairs=course.semester.pairs.generate(), course.preferences=NULL, min.per.semester=2, max.per.semester=5, max.semesters = 9, max.courses=16, include.summers=FALSE) {
  if(!include.summers) {
    course.semester.pairs <- course.semester.pairs[!grepl(".05", course.semester.pairs$Semester),]
  }
  semesters <- unique(course.semester.pairs$Semester)
  if(is.null(course.preferences)) {
    course.preferences <- rep(0.5, length(unique(course.semester.pairs$Course)))
    names(course.preferences) <- unique(course.semester.pairs$Course)
  }
  course.semester.pairs$Weight <- course.preferences[match(course.semester.pairs$Course, names(course.preferences))]
  schedule <- data.frame(matrix(nrow=0, ncol=2))
  for(semester.index in sequence(min(length(semesters), max.semesters))) {
    local.schedule <- subset(course.semester.pairs, course.semester.pairs$Semester==semesters[semester.index])
    if(nrow(local.schedule)>0) {
      number.courses.to.pick <- floor(runif(1, min.per.semester, min(nrow(local.schedule),max.per.semester)+1))
      selected.courses <- sample.int(n=nrow(local.schedule), size=min(number.courses.to.pick, nrow(local.schedule), na.rm=TRUE),prob=local.schedule$Weight)
      if(semester.index==1) { #hardcoding 150 just to start
        selected.courses <- which(grepl("BIO 150", local.schedule$Course))
      }
      if(semester.index==2) { #hard coding the 200 level required courses
        selected.courses <- c(
          which(local.schedule$Course=="(BIO 260) Ecology"),
          which(local.schedule$Course=="(BIO 269) Ecology Lab"),
          which(local.schedule$Course=="(BIO 280) Evolution"),
          which(local.schedule$Course=="(BIO 281) Evolution Lab")
        )
      }
      schedule <- rbind(schedule, local.schedule[selected.courses,])
      all.matches <- which(course.semester.pairs$Course %in% schedule$Course)
      all.matches <- all.matches[!is.na(all.matches)]
      if(length(all.matches)>0) {
        course.semester.pairs <- course.semester.pairs[-all.matches,]
      }
      if(semester.index==1) {
        course.semester.pairs <- course.semester.pairs[-which(grepl("BIO 1", course.semester.pairs$Course)),] #only take one Bio 100 level course
      }
    }
  }
  schedule <- schedule[1:min(nrow(schedule),max.courses),]
  return(schedule)
}

score.schedule <- function(schedule, prereqs=read.prereqs(), coreqs=read.coreqs()) {
  total.score <- mean(schedule$Weight)
  for (prereq.index in sequence(nrow(prereqs))) {
    course.row <- match(prereqs$Course[prereq.index], schedule$Course) 
    course.semester <- schedule$Semester[course.row]
    if(!is.na(course.row)) {
      prereq.row <- match(prereqs$Prereq[prereq.index], schedule$Course) 
      if(is.na(prereq.row)) {
        return(0) 
      } else {
        prereq.semester <-  schedule$Semester[prereq.row]
        if(as.numeric(prereq.semester) >= as.numeric(course.semester)) {
          return(0)
        }
      }
    }
  }
  for (coreq.index in sequence(nrow(coreqs))) {
    course.row <- match(coreqs$Course[coreq.index], schedule$Course) 
    course.semester <- schedule$Semester[course.row]
    if(!is.na(course.row)) {
      coreq.row <- match(coreqs$Coreq[coreq.index], schedule$Course) 
      if(is.na(coreq.row)) {
        return(0) 
      } else {
        coreq.semester <-  schedule$Semester[coreq.row]
        if(as.numeric(coreq.semester) != as.numeric(course.semester)) {
          return(0)
        }
      }
    }
  }
  return(total.score)
}

find.valid.schedule <- function(course.semester.pairs=course.semester.pairs.generate(), course.preferences=NULL, min.per.semester=2, max.per.semester=5, max.semesters = 9, max.courses=16, include.summers=FALSE, prereqs=read.prereqs(), coreqs=read.coreqs()) {
  score <- 0
  schedule <- NULL
  attempts <- 0
  while(score==0) {
    attempts <- attempts + 1
    schedule <- generate.schedule(course.semester.pairs, course.preferences, min.per.semester, max.per.semester, max.semesters, max.courses, include.summers)
    score <- score.schedule(schedule, prereqs, coreqs)
  }
  return(list(schedule=schedule, score=score, attempts=attempts))
}

optimize.valid.schedule <- function(course.semester.pairs=course.semester.pairs.generate(), course.preferences=NULL, min.per.semester=2, max.per.semester=5, max.semesters = 9, max.courses=16, include.summers=FALSE, prereqs=read.prereqs(), coreqs=read.coreqs(), max.starts=10) {
  best.score <- 0
  best.schedule <- NULL
  attempts <- 0
  starts <- 0
  while(starts < max.starts) {
    starts <- starts + 1
    result <- find.valid.schedule(course.semester.pairs, course.preferences, min.per.semester, max.per.semester, max.semesters, max.courses, include.summers, prereqs, coreqs)
    attempts <- attempts + result$attempts
    if(result$score>best.score) {
      best.schedule <- result$schedule
      best.score <- result$score
    }
  }
  return(list(schedule=best.schedule, score=best.score, attempts=attempts))
}



  course.semester.pairs<-course.semester.pairs.generate()
  
  courses <- sort(unique(course.semester.pairs$Course))
  names(courses) <- courses
  course.semester.pairs.to.prioritize <- course.semester.pairs[which(as.numeric(course.semester.pairs$Semester)>2019.6),]
  priority.courses <- sort(unique(course.semester.pairs.to.prioritize$Course))
  names(priority.courses) <- priority.courses
  priority.courses <- courses[-which(grepl("BIO", priority.courses))]
  ui <- fluidPage(
    
    sidebarLayout(position = "left",
                  
                  sidebarPanel(
                    sliderInput("max.courses", "Maximum number of EEB/Bio courses during undergrad:",
                                min = 6, max = 20,
                                value = 14, step=1),
                    sliderInput("courses.per.semester", "Min and max EEB courses per semester:",
                                min = 0, max = 5,
                                value = c(1,3), step=1),
                    checkboxGroupInput("variable", "Courses to prioritize:",
                                       choices=priority.courses )          
                  ),
                  mainPanel(
                    tableOutput("data")
                  )
    )
    
  )
  
  server <- function(input, output) {
    output$data <- renderTable({
      course.semester.pairs<-course.semester.pairs.generate()
      courses <- sort(unique(course.semester.pairs$Course))  
      course.preferences <- rep(0.01, length(courses))
      names(course.preferences) <- courses
      course.preferences[input$variable] <- 1
      final.result <- optimize.valid.schedule(course.preferences=course.preferences, max.courses=as.integer(input$max.courses), min.per.semester=as.integer(input$courses.per.semester)[1], max.per.semester=as.integer(input$courses.per.semester)[2])
      final.result$schedule$Semester <- sapply(final.result$schedule$Semester, convert.to.text.semester)
      final.result$schedule$Priority <- ifelse(final.result$schedule$Weight==1.0,"Yes", "")
      final.result$schedule <- final.result$schedule[,-which(colnames(final.result$schedule)=="Weight")]
      final.result$schedule
    }, rownames = FALSE)
  }
  
  shinyApp(ui, server)
