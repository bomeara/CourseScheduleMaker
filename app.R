#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)

read.course.info <- function(file="EEBCourses.tsv") {
  course.info <- read.delim(file,stringsAsFactors=FALSE)
  course.info <- subset(course.info, !grepl("N", course.info$Counts.for.EEB))
  return(course.info)
}

read.schedule <- function(file="EEBScheduleNew.txt") {
  schedule <- read.delim(file,stringsAsFactors=FALSE, row.names=1)
  return(schedule)
}

extract.course.number <- function(x) {
  #return(gsub("\\(", '', gsub("\\)", '', str_extract(x, "\\(.*\\)"))))\
  return(substr(x, 1, 7))
}


# course.semester.pairs.generate <- function(schedule=read.schedule()) {
#   course.grid <- schedule
#   colnames(course.grid) <- sapply(colnames(course.grid), convert.to.numeric.semester)
#   course.semester.pairs <- data.frame(matrix(nrow=0, ncol=3))
#   for (semester.index in sequence(ncol(course.grid))) {
#     for (course.index in sequence(nrow(course.grid))) {
#       if(!is.na(course.grid[course.index, semester.index])) {
#         course.semester.pairs <- rbind(course.semester.pairs, c(extract.course.number(course.grid[course.index,semester.index]), course.grid[course.index,semester.index], colnames(course.grid)[semester.index]), stringsAsFactors=FALSE)
#       }
#     }
#   }
#   colnames(course.semester.pairs) <- c("Course.Number", "Course", "Semester")
#   course.semester.pairs <- course.semester.pairs[order(course.semester.pairs$Semester),]
#   course.semester.pairs$Semester <- as.character(course.semester.pairs$Semester)
#   course.semester.pairs <- course.semester.pairs[which(nchar(course.semester.pairs$Course)>0),]
#   return(course.semester.pairs)
# }


course.semester.pairs.generate <- function(schedule=read.schedule(), courses=read.course.info()) {
	course.grid <- schedule
	colnames(course.grid) <- sapply(colnames(course.grid), convert.to.numeric.semester)
	course.semester.pairs <- data.frame(matrix(nrow=0, ncol=3))
	for (semester.index in sequence(ncol(course.grid))) {
		for (course.index in sequence(nrow(course.grid))) {
			if(nchar(course.grid[course.index, semester.index])>0) {
			course.semester.pairs <- rbind(course.semester.pairs, c(extract.course.number(rownames(course.grid)[course.index]), rownames(course.grid)[course.index], colnames(course.grid)[semester.index]), stringsAsFactors=FALSE)
			}
		}
	}
	colnames(course.semester.pairs) <- c("Course.Number", "Course", "Semester")
	course.semester.pairs <- course.semester.pairs[order(course.semester.pairs$Semester),]
	course.semester.pairs$Semester <- as.character(course.semester.pairs$Semester)
	course.semester.pairs <- course.semester.pairs[which(nchar(course.semester.pairs$Course)>0),]
	course.semester.pairs <- course.semester.pairs[course.semester.pairs$Course.Number %in%  courses$Course.Number,]
	course.semester.pairs <- course.semester.pairs[which(as.numeric(course.semester.pairs$Semester)>as.numeric(format(Sys.time(), "%Y.%m"))),]
	
	return(course.semester.pairs)
}




convert.to.numeric.semester <- function(semester) {
  year <- strsplit(semester, "\\.")[[1]][2]
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
  if(grepl("\\.08", semester)) {
    prefix <- "Fall"
  }
  if(grepl("\\.01", semester)) {
    prefix <- "Spring"
  }
  if(grepl("\\.05", semester)) {
    prefix <- "Summer"
  }
  return(paste0(prefix, " ", year))
}

generate.prereq.table <- function(course.info = read.course.info()) {
  course.info$Prereqs <- gsub("BIOL", "BIO", course.info$Prereqs)
  relevant.rows <- which(nchar(course.info$Prereqs)>0)
  return(course.info[relevant.rows,c("Course.Number", "Prereqs")])
}

generate.coreq.table <- function(course.info = read.course.info()) {
  course.info$Coreqs <- gsub("BIOL", "BIO", course.info$Coreqs)
  relevant.rows <- which(nchar(course.info$Coreqs)>0)
  return(course.info[relevant.rows,c("Course.Number", "Coreqs")])
}

# generate.track.vector <- function(course.info = read.course.info()) {
#   tracks <- unique(unlist(strsplit(course.info$Tracks, split="\\,\\s+")))
#   track.vector <- rep("",length(tracks))
#   names(track.vector) <- tracks
#   for (i in sequence(length(tracks))) {
#     track.vector[i] <- paste0(course.info$Course.Number[grepl(names(track.vector)[i], course.info$Tracks)], collapse=", ")
#   }
#   return(track.vector)
# }

generate.track.vector <- function(course.info = read.course.info()) {
	trackcols <- which(grepl("Focus", colnames(course.info)))
	tracks <- colnames(course.info)[trackcols]
	tracks <- gsub("\\.\\.\\.", " and ", tracks)
	tracks <- gsub("Focus\\.\\.", "", tracks)
	tracks <- gsub("\\.", " ", tracks)
	track.vector <- rep("",length(tracks))
  	names(track.vector) <- tracks
  	for (i in sequence(length(tracks))) {
    	track.vector[i] <- paste0(course.info$Course.Number[nchar(course.info[,trackcols[i]])>0], collapse=", ")
  	}
  	return(track.vector)
}


#course.preferences: weight of taking that course (from 0 to 1)
generate.schedule <- function(course.semester.pairs=course.semester.pairs.generate(), course.preferences=NULL, min.per.semester=2, max.per.semester=5, max.semesters = 9, max.courses=16, include.summers=FALSE, allow.500=FALSE) {
  if(!include.summers) {
    course.semester.pairs <- course.semester.pairs[!grepl(".05", course.semester.pairs$Semester),]
  }
  if(!allow.500) {
    course.semester.pairs <- course.semester.pairs[!grepl("5[[:digit:]][[:digit:]]", course.semester.pairs$Course.Number),]
  }
    course.semester.pairs <- course.semester.pairs[!grepl("6[[:digit:]][[:digit:]]", course.semester.pairs$Course.Number),] #get rid of 600 level

  semesters <- unique(course.semester.pairs$Semester)
  if(is.null(course.preferences)) {
    course.preferences <- rep(0.5, length(unique(course.semester.pairs$Course)))
    names(course.preferences) <- unique(course.semester.pairs$Course)
  }
  course.semester.pairs$Weight <- course.preferences[match(course.semester.pairs$Course, names(course.preferences))]
  schedule <- data.frame(matrix(nrow=0, ncol=2))
  for(semester.index in sequence(min(length(semesters), max.semesters))) {
    local.schedule <- subset(course.semester.pairs, course.semester.pairs$Semester==semesters[semester.index])
	print(semester.index)
	print(local.schedule)
    if(nrow(local.schedule)>0) {
      number.courses.to.pick <- floor(runif(1, min.per.semester, min(nrow(local.schedule),max.per.semester)+1))
      selected.courses <- sample.int(n=nrow(local.schedule), size=min(number.courses.to.pick, nrow(local.schedule), na.rm=TRUE),prob=local.schedule$Weight)
    #   if(semester.index==1) { #hardcoding 150 just to start
    #     selected.courses <- which(grepl("BIO 150", local.schedule$Course))
    #   }
    #   if(semester.index==2) { #hard coding the 200 level required courses
    #     selected.courses <- c(
    #       which(local.schedule$Course=="(BIO 260) Ecology"),
    #       which(local.schedule$Course=="(BIO 269) Ecology Lab"),
    #       which(local.schedule$Course=="(BIO 280) Evolution"),
    #       which(local.schedule$Course=="(BIO 281) Evolution Lab")
    #     )
    #   }
	  print(local.schedule[selected.courses,])
      schedule <- rbind(schedule, local.schedule[selected.courses,])
      all.matches <- which(course.semester.pairs$Course %in% schedule$Course)
      all.matches <- all.matches[!is.na(all.matches)]
      if(length(all.matches)>0) {
        course.semester.pairs <- course.semester.pairs[-all.matches,]
      }
     # if(semester.index==1) {
     #   course.semester.pairs <- course.semester.pairs[-which(grepl("BIO 1", course.semester.pairs$Course)),] #only take one Bio 100 level course
     # }
    }
  }
  schedule <- schedule[1:min(nrow(schedule),max.courses),]
  return(schedule)
}

score.schedule <- function(schedule, prereqs=generate.prereq.table(), coreqs=generate.coreq.table()) {
  total.score <- mean(schedule$Weight)
  # Take out prereqs for now

  # for (prereq.index in sequence(nrow(prereqs))) {
  #   course.row <- match(prereqs$Course[prereq.index], schedule$Course.Number)
  #   course.semester <- schedule$Semester[course.row]
  #   if(!is.na(course.row)) {
  #     prereq.row <- match(prereqs$Prereq[prereq.index], schedule$Course.Number)
  #     if(is.na(prereq.row)) {
  #       return(0)
  #     } else {
  #       prereq.semester <-  schedule$Semester[prereq.row]
  #       if(as.numeric(prereq.semester) >= as.numeric(course.semester)) {
  #         return(0)
  #       }
  #     }
  #   }
  # }
  # for (coreq.index in sequence(nrow(coreqs))) {
  #   course.row <- match(coreqs$Course.Number[coreq.index], schedule$Course.Number)
  #   course.semester <- schedule$Semester[course.row]
  #   if(!is.na(course.row)) {
  #     coreq.row <- match(coreqs$Coreq.Number[coreq.index], schedule$Course.Number)
  #     if(is.na(coreq.row)) {
  #       return(0)
  #     } else {
  #       coreq.semester <-  schedule$Semester[coreq.row]
  #       if(as.numeric(coreq.semester) != as.numeric(course.semester)) {
  #         return(0)
  #       }
  #     }
  #   }
  # }
  return(total.score)
}

find.valid.schedule <- function(course.semester.pairs=course.semester.pairs.generate(), course.preferences=NULL, min.per.semester=2, max.per.semester=5, max.semesters = 9, max.courses=16, include.summers=FALSE, allow.500=FALSE, prereqs=read.prereqs(), coreqs=read.coreqs()) {
  score <- 0
  schedule <- NULL
  attempts <- 0
  while(score==0) {
    attempts <- attempts + 1
    schedule <- generate.schedule(course.semester.pairs, course.preferences, min.per.semester, max.per.semester, max.semesters, max.courses, include.summers, allow.500)
    score <- score.schedule(schedule, prereqs, coreqs)
  }
  return(list(schedule=schedule, score=score, attempts=attempts))
}

optimize.valid.schedule <- function(course.semester.pairs=course.semester.pairs.generate(), course.preferences=NULL, min.per.semester=2, max.per.semester=5, max.semesters = 9, max.courses=16, include.summers=FALSE, allow.500=FALSE, prereqs=read.prereqs(), coreqs=read.coreqs(), max.starts=10) {
  best.score <- 0
  best.schedule <- NULL
  attempts <- 0
  starts <- 0
  while(starts < max.starts) {
    starts <- starts + 1
    result <- find.valid.schedule(course.semester.pairs, course.preferences, min.per.semester, max.per.semester, max.semesters, max.courses, include.summers, allow.500, prereqs, coreqs)
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
  course.semester.pairs.to.prioritize <- course.semester.pairs[which(as.numeric(course.semester.pairs$Semester)>as.numeric(format(Sys.time(), "%Y.%m"))),]
  priority.courses <- sort(unique(course.semester.pairs.to.prioritize$Course))
  names(priority.courses) <- priority.courses
#  priority.courses <- courses[-which(grepl("BIO", priority.courses))]
  track.vector <- generate.track.vector()
  ui <- fluidPage(
    titlePanel("EEB undergraduate potential schedule generator"),
    sidebarLayout(position = "left",

                  sidebarPanel(
                    sliderInput("max.courses", "Maximum number of EEB/Bio courses during undergrad:",
                                min = 6, max = 20,
                                value = 14, step=1),
                    sliderInput("courses.per.semester", "Min and max EEB courses per semester:",
                                min = 0, max = 5,
                                value = c(1,3), step=1),
                    h4("Other constraints"),
                    checkboxInput("summer", "Allow summer courses", FALSE),
                    checkboxInput("level500", "Allow 500 level courses", FALSE),
                    checkboxGroupInput("tracks", "Focus, if any, to prioritize:",
                                choices=names(track.vector)),
                    checkboxGroupInput("variable", "Courses to prioritize:",
                                       choices=priority.courses )
                  ),
                  mainPanel(
					  p("To use this tool to plan EEB courses over the next 4 semesters, select options on the left and it will create a schedule for you. You can try changing settings to generate new possible schedules. Confer with your advisor to check your schedule to make sure it meets all the requirements for the concentration and graduation in general. This tool is designed to be helpful in planning out the future (and for those not yet in EEB, figuring out what you could do in EEB). If there are any questions about the tool, reach out to Brian O'Meara, bomeara@utk.edu."),
					  p("This tool includes EEB courses that give credit within the concentration. Students are encouraged to consider approved courses in departments such as anthropology, biochemistry, chemistry, forestry, geography, geology, microbiology and wildlife & fisheries that also meet EEB requirements, though their five year schedule is not available (the list of approved courses in other departments is available in the catalog). Note that this tool is based on EEB's five year course plan, but it is subject to change: we may offer additional courses, a faculty member may go on development or family leave, someone may retire early, and so forth. It also does not check to make sure there are no course schedule overlaps during a semester -- we try to avoid these, but they must still sometimes occur."),
                    tableOutput("data")
                  )
    )

  )

  server <- function(input, output) {
    output$data <- renderTable({
      course.semester.pairs<-course.semester.pairs.generate()
      #course.semester.pairs <- course.semester.pairs[which(as.numeric(course.semester.pairs$Semester)>as.numeric(format(Sys.time(), "%Y.%m"))),]
      
      track.vector <- generate.track.vector()
      courses <- sort(unique(course.semester.pairs$Course))
      course.preferences <- rep(0.01, length(courses))
      names(course.preferences) <- courses
      course.preferences[input$variable] <- 1
      if(length(input$tracks)>0) {
        relevant.courses <- strsplit(paste0(track.vector[input$tracks], collapse=", "), ", ")[[1]]
        for(course.index in sequence(length(relevant.courses))) {
          matching.course <- which(grepl(relevant.courses[course.index], names(course.preferences)))
          if(length(matching.course)==1) {
            course.preferences[matching.course] <- course.preferences[matching.course]+.3
          }
        }
      }
      final.result <- optimize.valid.schedule(course.preferences=course.preferences, max.courses=as.integer(input$max.courses), min.per.semester=as.integer(input$courses.per.semester)[1], max.per.semester=as.integer(input$courses.per.semester)[2], include.summers = input$summer, allow.500=input$level500)
      final.result$schedule$Semester <- sapply(final.result$schedule$Semester, convert.to.text.semester)
      final.result$schedule$Priority <- ifelse(final.result$schedule$Weight>0.1,"Yes", "")
      final.result$schedule <- final.result$schedule[,-which(colnames(final.result$schedule)=="Weight")]
      final.result$schedule
    }, rownames = FALSE)
  }

  shinyApp(ui, server)

