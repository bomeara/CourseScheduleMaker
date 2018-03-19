#' Read and process course data
read.courses <- function(file="Courses.txt") {
  courses <- as.data.frame(readit::readit(file),stringsAsFactors=FALSE)
  course.names <- unique(unlist(courses))
  course.names <- course.names[!is.na(course.names)]
  return(list(course.grid = courses, course.names=course.names))
}

course.semester.pairs.generate <- function(course.grid) {
  course.semester.pairs <- data.frame(matrix(nrow=0, ncol=2))
  for (semester.index in sequence(ncol(course.grid))) {
    for (course.index in sequence(nrow(course.grid))) {
      if(!is.na(course.grid[course.index, semester.index])) {
        course.semester.pairs <- rbind(course.semester.pairs, c(course.grid[course.index,semester.index], colnames(course.grid)[semester.index]), stringsAsFactors=FALSE)
      }
    }
  }
  colnames(course.semester.pairs) <- c("Course", "Semester")
  return(course.semester.pairs)
}

read.prereqs <- function(file="Prereq.txt") {
  prereqs <- as.data.frame(readit::readit(file),stringsAsFactors=FALSE)
  return(prereqs)
}

read.coreqs <- function(file="Coreq.txt") {
  coreqs <- as.data.frame(readit::readit(file),stringsAsFactors=FALSE)
  return(coreq)
}


#course.preferences: weight of taking that course (from 0 to 1)
generate.schedule <- function(course.semester.pairs, course.preferences=NULL, min.per.semester=2, max.per.semester=5) {
  semesters <- unique(course.semester.pairs$Semester)
  if(is.null(course.preferences)) {
    course.preferences <- rep(1, length(unique(course.semester.pairs$Course)))
    names(course.preferences) <- unique(course.semester.pairs$Course)
  }
  course.semester.pairs$Weight <- course.preferences[match(course.semester.pairs$Course, names(course.preferences))]
  schedule <- data.frame(matrix(nrow=0, ncol=2))
  for(semester.index in sequence(length(semesters))) {
    local.schedule <- subset(course.semester.pairs, course.semester.pairs$Semester==semesters[semester.index])
    number.courses.to.pick <- floor(runif(1, min.per.semester, min(nrow(local.schedule),max.per.semester)+1))
    selected.courses <- sample.int(n=nrow(local.schedule), size=min(number.courses.to.pick, nrow(local.schedule)),prob=local.schedule$Weight)
    schedule <- rbind(schedule, local.schedule[selected.courses,])
    all.matches <- match(schedule$Course, course.semester.pairs$Course)
    all.matches <- all.matches[is.na(all.matches)]
    if(length(all.matches)>0) {
      course.semester.pairs <- course.semester.pairs[-all.matches,]
    }
  }
  return(schedule)
}
